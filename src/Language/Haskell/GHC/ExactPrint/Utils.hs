{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-} -- Suspect GHC bug to need this.
module Language.Haskell.GHC.ExactPrint.Utils
  (
    annotateLHsModule

  , ghcIsComment
  , ghcIsMultiLine

  , srcSpanStartLine
  , srcSpanEndLine
  , srcSpanStartColumn
  , srcSpanEndColumn

  , ss2span
  , ss2pos
  , ss2posEnd
  , undelta
  , undeltaComment
  , isGoodDelta
  , rdrName2String
  , isSymbolRdrName

  , isListComp

  , showGhc

  , merge

  -- * For tests
  , debug

  , runAP
  , AP(..)
  , getSrcSpanAP, pushSrcSpanAP, popSrcSpanAP
  , getAnnotationAP
  , addAnnotationsAP

  , ghead
  , glast
  , gtail
  , gfromJust

  ) where

import Control.Applicative (Applicative(..))
import Control.Monad (when, liftM, ap)
import Control.Exception
import Data.Data
import Data.List
import Data.Maybe
import Data.Monoid
import Language.Haskell.GHC.GenericInstances

import Language.Haskell.GHC.ExactPrint.Types

import qualified Bag            as GHC
import qualified BasicTypes     as GHC
import qualified BooleanFormula as GHC
import qualified Class          as GHC
import qualified CoAxiom        as GHC
import qualified DynFlags       as GHC
import qualified FastString     as GHC
import qualified ForeignCall    as GHC
import qualified GHC            as GHC
import qualified GHC.Paths      as GHC
import qualified Lexer          as GHC
import qualified Name           as GHC
import qualified NameSet        as GHC
import qualified Outputable     as GHC
import qualified RdrName        as GHC
import qualified SrcLoc         as GHC
import qualified StringBuffer   as GHC
import qualified UniqSet        as GHC
import qualified Unique         as GHC
import qualified Var            as GHC

import qualified Data.Map as Map

import Debug.Trace

debug :: c -> String -> c
-- debug = flip trace
debug c _ = c

-- ---------------------------------------------------------------------

-- | Type used in the AP Monad. The state variables maintain
--    - the current SrcSpan and the TypeRep of the thing it encloses
--      as a stack to the root of the AST as it is traversed,
--    - the srcspan of the last thing annotated, to calculate delta's from
--    - extra data needing to be stored in the monad
--    - the annotations provided by GHC

{- -}
newtype AP x = AP ([(GHC.SrcSpan,AnnConName)] -> GHC.SrcSpan -> Extra -> GHC.ApiAnns
            -> (x, [(GHC.SrcSpan,AnnConName)],   GHC.SrcSpan,   Extra,   GHC.ApiAnns,
                  ([(AnnKey,Annotation)],[(AnnKeyF,[DeltaPos])])
                 ))

type Extra = Bool -- isInfix for a FunBind

instance Functor AP where
  fmap = liftM

instance Applicative AP where
  pure = return
  (<*>) = ap

instance Monad AP where
  return x = AP $ \l pe e ga -> (x, l, pe, e, ga, mempty)

  AP m >>= k = AP $ \l0 p0 e0 ga0 -> let
        (a, l1, p1, e1, ga1, s1) = m l0 p0 e0 ga0
        AP f = k a
        (b, l2, p2, e2, ga2, s2) = f l1 p1 e1 ga1
    in (b, l2, p2, e2, ga2, s1 <> s2)


runAP :: AP () -> GHC.ApiAnns -> Anns
runAP (AP f) ga
 = let (_,_,_,_,_,(se,sa)) = f [] GHC.noSrcSpan False ga
   in (Map.fromListWith combineAnns se,Map.fromListWith (++) sa)
        --  `debug` ("runAP:se=" ++ show se)

combineAnns :: Annotation -> Annotation -> Annotation
combineAnns (Ann cs1 dp1) (Ann cs2 _) = Ann (cs1 ++ cs2) dp1

-- -------------------------------------

-- |Note: assumes the SrcSpan stack is nonempty
getSrcSpanAP :: AP GHC.SrcSpan
getSrcSpanAP = AP (\l pe e ga -> (fst $ ghead "getSrcSpanAP" l,l,pe,e,ga,mempty))

pushSrcSpanAP :: (GenericCon a) => (GHC.Located a) -> AP ()
pushSrcSpanAP (GHC.L l a) = AP (\ls pe e ga -> ((),(l,annGetConstr a):ls,pe,e,ga,mempty))

popSrcSpanAP :: AP ()
popSrcSpanAP = AP (\(_:ls) pe e ga -> ((),ls,pe,e,ga,mempty))

-- ---------------------------------------------------------------------

-- |Note: assumes the prior end SrcSpan stack is nonempty
getPriorEnd :: AP GHC.SrcSpan
getPriorEnd = AP (\l pe e ga -> (pe,l,pe,e,ga,mempty))

setPriorEnd :: GHC.SrcSpan -> AP ()
setPriorEnd pe = AP (\ls _ e ga  -> ((),ls,pe,e,ga,mempty))

-- Deprecated, remove
popPriorEnd :: AP ()
popPriorEnd = AP (\ls pe e ga -> ((),ls,pe,e,ga,mempty)
 `debug` ("popPriorEnd: old stack :" ++ showGhc pe))

-- -------------------------------------

getAnnotationAP :: GHC.SrcSpan -> GHC.AnnKeywordId -> AP [GHC.SrcSpan]
getAnnotationAP sp an = AP (\l pe e ga
    -> (GHC.getAnnotation ga sp an, l,pe,e,ga,mempty))


getAndRemoveAnnotationAP :: GHC.SrcSpan -> GHC.AnnKeywordId -> AP [GHC.SrcSpan]
getAndRemoveAnnotationAP sp an = AP (\l pe e ga ->
    let
      (r,ga') = GHC.getAndRemoveAnnotation ga sp an
    in (r, l,pe,e,ga',mempty))

{-
-- temporary, moved to GHC
getAndRemoveAnnotation :: GHC.ApiAnns -> GHC.SrcSpan -> GHC.AnnKeywordId
                       -> ([GHC.SrcSpan],GHC.ApiAnns)
getAndRemoveAnnotation (anns,cs) span ann
   = case Map.lookup (span,ann) anns of
       Nothing -> ([],(anns,cs))
       Just ss -> (ss,(Map.delete (span,ann) anns,cs))
-}

-- -------------------------------------
-- |Retrieve the comments allocated to the current 'SrcSpan', and
-- remove them from the annotations
getAndRemoveAnnotationComments :: GHC.ApiAnns -> GHC.SrcSpan
                               -> ([GHC.Located GHC.AnnotationComment],GHC.ApiAnns)
getAndRemoveAnnotationComments (anns,canns) span =
  (case Map.lookup span canns of
    Just cs -> (cs,(anns,Map.delete span canns))
    Nothing -> ([],(anns,canns)))
     `debug` ("getAndRemoveAnnotationComments:span=" ++ showGhc span)

----------------------------------------

getCommentsForSpan :: GHC.SrcSpan -> AP [Comment]
getCommentsForSpan s = AP (\l pe e ga ->
  let
    (gcs,ga1) = getAndRemoveAnnotationComments ga s
    cs = reverse $ map tokComment gcs
    tokComment :: GHC.Located GHC.AnnotationComment -> Comment
    tokComment t@(GHC.L lt _) = Comment (ghcIsMultiLine t) (ss2span lt) (ghcCommentText t)
  in (cs,l,pe,e,ga1,mempty)
      `debug` ("getCommentsForSpan:(s,cs)" ++ show (showGhc s,cs))
     )

-- -------------------------------------

-- |Add some annotation to the currently active SrcSpan
addAnnotationsAP :: Annotation -> AP ()
addAnnotationsAP ann = AP (\l pe e ga ->
                       ( (),l,pe,e,ga,
                 ([((ghead "addAnnotationsAP" l),ann)],[])))

-- -------------------------------------

addAnnDeltaPos :: (GHC.SrcSpan,KeywordId) -> DeltaPos -> AP ()
addAnnDeltaPos (s,kw) dp = AP (\l pe e ga -> ( (),
                                l,pe,e,ga,
                               ([],
                               [ ((s,kw),[dp]) ])  ))

-- -------------------------------------

setFunIsInfix :: Bool -> AP ()
setFunIsInfix e = AP (\l pe _ ga -> ((),l,pe,e,ga,mempty))

getFunIsInfix :: AP Bool
getFunIsInfix = AP (\l pe e ga -> (e,l,pe,e,ga,mempty))

-- -------------------------------------

-- | Enter a new AST element. Maintain SrcSpan stack
enterAST :: (GenericCon a) => GHC.Located a -> AP ()
enterAST lss = do
  return () `debug` ("enterAST entered for " ++ show (ss2span $ GHC.getLoc lss))
  pushSrcSpanAP lss
  return ()


-- | Pop up the SrcSpan stack, capture the annotations, and work the
-- comments in belonging to the span
-- Assumption: the annotations belong to the immediate sub elements of
-- the AST, hence relate to the current SrcSpan. They can thus be used
-- to decide which comments belong at this level,
-- The assumption is made valid by matching enterAST/leaveAST calls.
leaveAST :: AP ()
leaveAST = do
  -- Automatically add any trailing comma or semi
  addDeltaAnnotationAfter GHC.AnnComma
  ss <- getSrcSpanAP
  if ss2span ss == ((1,1),(1,1))
     then return ()
     else addDeltaAnnotationsOutside GHC.AnnSemi AnnSemiSep

  priorEnd <- getPriorEnd

  newCs <- getCommentsForSpan ss
  let (lcs,_) = localComments (ss2span ss) newCs []

  let dp = deltaFromSrcSpans priorEnd ss
  addAnnotationsAP (Ann lcs dp) `debug` ("leaveAST:(ss,lcs)=" ++ show (showGhc ss,lcs))
  popSrcSpanAP
  return () `debug` ("leaveAST:(ss,dp,priorEnd)=" ++ show (ss2span ss,dp,ss2span priorEnd))

-- ---------------------------------------------------------------------

class (GenericCon ast) => AnnotateP ast where
  annotateP :: GHC.SrcSpan -> ast -> AP ()

-- |First move to the given location, then call exactP
annotatePC :: (AnnotateP ast) => GHC.Located ast -> AP ()
annotatePC a@(GHC.L l ast) = do
  enterAST a `debug` ("annotatePC:entering " ++ showGhc l)
  annotateP l ast
  leaveAST `debug` ("annotatePC:leaving " ++ showGhc (l))

annotateMaybe :: (AnnotateP ast) => Maybe (GHC.Located ast) -> AP ()
annotateMaybe Nothing    = return ()
annotateMaybe (Just ast) = annotatePC ast

annotateList :: (AnnotateP ast) => [GHC.Located ast] -> AP ()
annotateList xs = mapM_ annotatePC xs

-- ---------------------------------------------------------------------

isGoodDelta (DP (ro,co)) = ro >= 0 && co >= 0

addFinalComments :: AP ()
addFinalComments = do
  return () `debug` ("addFinalComments:entering=")
  cs <- getCommentsForSpan GHC.noSrcSpan
  let (dcs,_) = localComments ((1,1),(1,1)) cs []
  pushSrcSpanAP (GHC.L GHC.noSrcSpan ())
  addAnnotationsAP (Ann dcs (DP (0,0)))
    -- `debug` ("leaveAST:dcs=" ++ show dcs)
  return () `debug` ("addFinalComments:dcs=" ++ show dcs)

addAnnotationWorker :: KeywordId -> GHC.SrcSpan -> AP ()
addAnnotationWorker ann ap = do
  if not (isPointSrcSpan ap)
    then do
      pe <- getPriorEnd
      ss <- getSrcSpanAP
      let p = deltaFromSrcSpans pe ap
      case (ann,isGoodDelta p) of
        (G GHC.AnnComma,False) -> return ()
             `debug`  ("addDeltaAnnotationWorker::bad delta:(ss,ma,p,ann)=" ++ show (ss2span ss,ss2span ap,p,ann))
        (G GHC.AnnSemi,False) -> return ()
             `debug`  ("addDeltaAnnotationWorker::bad delta:(ss,ma,p,ann)=" ++ show (ss2span ss,ss2span ap,p,ann))
        (G GHC.AnnOpen,False) -> return ()
             `debug`  ("addDeltaAnnotationWorker::bad delta:(ss,ma,p,ann)=" ++ show (ss2span ss,ss2span ap,p,ann))
        (G GHC.AnnClose,False) -> return ()
             `debug`  ("addDeltaAnnotationWorker::bad delta:(ss,ma,p,ann)=" ++ show (ss2span ss,ss2span ap,p,ann))
        _ -> do
          addAnnDeltaPos (ss,ann) p
          setPriorEnd ap
              `debug` ("addDeltaAnnotationWorker:(ss,pe,ap,p,ann)=" ++ show (ss2span ss,ss2span pe,ss2span ap,p,ann))
    else do
      return ()
          `debug` ("addDeltaAnnotationWorker::point span:(ss,ma,ann)=" ++ show (ss2span ap,ann))

-- | Look up and add a Delta annotation at the current position, and
-- advance the position to the end of the annotation
addDeltaAnnotation :: GHC.AnnKeywordId -> AP ()
addDeltaAnnotation ann = do
  pe <- getPriorEnd
  ss <- getSrcSpanAP
  ma <- getAnnotationAP ss ann
  case nub ma of -- ++AZ++ TODO: get rid of duplicates earlier
    [] -> return () `debug` ("addDeltaAnnotation empty ma for:" ++ show ann)
    [ap] -> addAnnotationWorker (G ann) ap
    _ -> error $ "addDeltaAnnotation:(ss,ann,ma)=" ++ showGhc (ss,ann,ma)

-- | Look up and add a Delta annotation appearing beyond the current
-- SrcSpan at the current position, and advance the position to the
-- end of the annotation
addDeltaAnnotationAfter :: GHC.AnnKeywordId -> AP ()
addDeltaAnnotationAfter ann = do
  pe <- getPriorEnd
  ss <- getSrcSpanAP
  ma <- getAnnotationAP ss ann
  let ma' = filter (\s -> not (GHC.isSubspanOf s ss)) ma
  case ma' of
    [] -> return () `debug` ("addDeltaAnnotation empty ma")
    [ap] -> addAnnotationWorker (G ann) ap
    _ -> error $ "addDeltaAnnotation:(ss,ann,ma)=" ++ showGhc (ss,ann,ma)

-- | Look up and add a Delta annotation at the current position, and
-- advance the position to the end of the annotation
addDeltaAnnotationLs :: GHC.AnnKeywordId -> Int -> AP ()
addDeltaAnnotationLs ann off = do
  pe <- getPriorEnd
  ss <- getSrcSpanAP
  ma <- getAnnotationAP ss ann
  case (drop off ma) of
    [] -> return ()
        `debug` ("addDeltaAnnotationLs:missed:(off,pe,ann,ma)=" ++ show (off,ss2span pe,ann,fmap ss2span ma))
    (ap:_) -> addAnnotationWorker (G ann) ap

-- | Look up and add possibly multiple Delta annotation at the current
-- position, and advance the position to the end of the annotations
addDeltaAnnotations :: GHC.AnnKeywordId -> AP ()
addDeltaAnnotations ann = do
  ss <- getSrcSpanAP
  ma <- getAnnotationAP ss ann
  let do_one ap' = addAnnotationWorker (G ann) ap'
                    `debug` ("addDeltaAnnotations:do_one:(ap',ann)=" ++ showGhc (ap',ann))
  mapM_ do_one (sort ma)

-- | Look up and add possibly multiple Delta annotations enclosed by
-- the current SrcSpan at the current position, and advance the
-- position to the end of the annotations
addDeltaAnnotationsInside :: GHC.AnnKeywordId -> AP ()
addDeltaAnnotationsInside ann = do
  ss <- getSrcSpanAP
  ma <- getAnnotationAP ss ann
  let do_one ap' = addAnnotationWorker (G ann) ap'
                    `debug` ("addDeltaAnnotations:do_one:(ap',ann)=" ++ showGhc (ap',ann))
  mapM_ do_one (sort $ filter (\s -> GHC.isSubspanOf s ss) ma)

-- | Look up and add possibly multiple Delta annotations not enclosed by
-- the current SrcSpan at the current position, and advance the
-- position to the end of the annotations
addDeltaAnnotationsOutside :: GHC.AnnKeywordId -> KeywordId -> AP ()
addDeltaAnnotationsOutside gann ann = do
  ss <- getSrcSpanAP
  -- ma <- getAnnotationAP ss gann
  ma <- getAndRemoveAnnotationAP ss gann
  let do_one ap' = addAnnotationWorker ann ap'
                    `debug` ("addDeltaAnnotations:do_one:(ap',ann)=" ++ showGhc (ap',ann))
  mapM_ do_one (sort $ filter (\s -> not (GHC.isSubspanOf s ss)) ma)

-- | Add a Delta annotation at the current position, and advance the
-- position to the end of the annotation
addDeltaAnnotationExt :: GHC.SrcSpan -> GHC.AnnKeywordId -> AP ()
addDeltaAnnotationExt s ann = do
  pe <- getPriorEnd
  ss <- getSrcSpanAP
  let p = deltaFromSrcSpans pe s
  addAnnDeltaPos (ss,G ann) p
  setPriorEnd s

addEofAnnotation :: AP ()
addEofAnnotation = do
  pe <- getPriorEnd
  ss <- getSrcSpanAP
  ma <- getAnnotationAP GHC.noSrcSpan GHC.AnnEofPos
  case ma of
    [] -> return ()
    [ap] -> do
      let p = deltaFromSrcSpans pe ap
      addAnnDeltaPos (ss,G GHC.AnnEofPos) p
      setPriorEnd ap

countAnnsAP :: GHC.AnnKeywordId -> AP Int
countAnnsAP ann = do
  ss <- getSrcSpanAP
  ma <- getAnnotationAP ss ann
  return (length ma)

-- ---------------------------------------------------------------------
-- Managing lists which have been separated, e.g. Sigs and Binds

prepareListAnnotation :: AnnotateP a => [GHC.Located a] -> [(GHC.SrcSpan,AP ())]
prepareListAnnotation ls = map (\b@(GHC.L l _) -> (l,annotatePC b)) ls

applyListAnnotations :: [(GHC.SrcSpan,AP ())] -> AP ()
applyListAnnotations ls
  = mapM_ (\(_,b) -> b) $ sortBy (\(a,_) (b,_) -> compare a b) ls

-- ---------------------------------------------------------------------
-- Start of application specific part

-- ---------------------------------------------------------------------

annotateLHsModule :: GHC.Located (GHC.HsModule GHC.RdrName) -> GHC.ApiAnns
                  -> Anns
annotateLHsModule modu ghcAnns
   -- = runAP (annotatePC modu >> addFinalComments) ghcAnns
   = runAP (addFinalComments >> annotatePC modu) ghcAnns

-- ---------------------------------------------------------------------

instance AnnotateP (GHC.HsModule GHC.RdrName) where
  annotateP lm (GHC.HsModule mmn mexp imps decs mdepr _haddock) = do
    setPriorEnd lm

    addDeltaAnnotation GHC.AnnModule

    case mmn of
      Nothing -> return ()
      Just (GHC.L ln _) -> addDeltaAnnotationExt ln GHC.AnnVal

    annotateMaybe mdepr

    case mexp of
      Nothing -> return ()
      Just exp -> annotatePC exp

    addDeltaAnnotation GHC.AnnWhere
    addDeltaAnnotation GHC.AnnOpenC -- Possible '{'
    addDeltaAnnotations GHC.AnnSemi -- possible leading semis
    mapM_ annotatePC imps

    annotateList decs

    addDeltaAnnotation GHC.AnnCloseC -- Possible '}'

    addEofAnnotation


-- ---------------------------------------------------------------------

instance AnnotateP GHC.WarningTxt where
  annotateP l (GHC.WarningTxt (GHC.L ls _) lss) = do
    addDeltaAnnotationExt ls GHC.AnnOpen
    addDeltaAnnotation GHC.AnnOpenS
    mapM_ annotatePC lss
    addDeltaAnnotation GHC.AnnCloseS
    addDeltaAnnotation GHC.AnnClose

  annotateP l (GHC.DeprecatedTxt (GHC.L ls _) lss) = do
    addDeltaAnnotationExt ls GHC.AnnOpen
    addDeltaAnnotation GHC.AnnOpenS
    mapM_ annotatePC lss
    addDeltaAnnotation GHC.AnnCloseS
    addDeltaAnnotation GHC.AnnClose

-- ---------------------------------------------------------------------

instance AnnotateP [GHC.LIE GHC.RdrName] where
   annotateP l ls = do
     addDeltaAnnotation GHC.AnnHiding -- in an import decl
     addDeltaAnnotation GHC.AnnOpenP -- '('
     mapM_ annotatePC ls
     addDeltaAnnotation GHC.AnnCloseP -- ')'

instance AnnotateP (GHC.IE GHC.RdrName) where
  annotateP l ie = do

    case ie of
        (GHC.IEVar ln) -> do
          addDeltaAnnotation GHC.AnnPattern
          addDeltaAnnotation GHC.AnnType
          annotatePC ln

        (GHC.IEThingAbs ln) -> do
          addDeltaAnnotation GHC.AnnType
          annotatePC ln

        (GHC.IEThingWith ln ns) -> do
          annotatePC ln
          addDeltaAnnotation GHC.AnnOpenP
          mapM_ annotatePC ns
          addDeltaAnnotation GHC.AnnCloseP

        (GHC.IEThingAll ln) -> do
          annotatePC ln
          addDeltaAnnotation GHC.AnnOpenP
          addDeltaAnnotation GHC.AnnDotdot
          addDeltaAnnotation GHC.AnnCloseP

        (GHC.IEModuleContents (GHC.L lm n)) -> do
          addDeltaAnnotation GHC.AnnModule
          addDeltaAnnotationExt lm GHC.AnnVal

        x -> error $ "annotateP.IE: notimplemented for " ++ showGhc x


-- ---------------------------------------------------------------------

instance AnnotateP GHC.RdrName where
  annotateP l n = do
    case rdrName2String n of
      "[]" -> do
        addDeltaAnnotation GHC.AnnOpenS -- '[' nonBUG
        addDeltaAnnotation GHC.AnnCloseS -- ']' BUG
      "()" -> do
        addDeltaAnnotation GHC.AnnOpenP -- '('
        addDeltaAnnotation GHC.AnnCloseP -- ')'
      "(##)" -> do
        addDeltaAnnotation GHC.AnnOpen -- '(#'
        addDeltaAnnotation GHC.AnnClose -- '#)'
      "[::]" -> do
        addDeltaAnnotation GHC.AnnOpen -- '[:'
        addDeltaAnnotation GHC.AnnClose -- ':]'
      _ ->  do
        addDeltaAnnotation GHC.AnnType
        addDeltaAnnotation GHC.AnnOpenP -- '('
        addDeltaAnnotationLs GHC.AnnBackquote 0
        addDeltaAnnotations GHC.AnnCommaTuple -- For '(,,,)'
        cnt <- countAnnsAP GHC.AnnVal
        cntT <- countAnnsAP GHC.AnnCommaTuple
        cntR <- countAnnsAP GHC.AnnRarrow
        case cnt of
          0 -> if cntT >0 || cntR >0 then return () else addDeltaAnnotationExt l GHC.AnnVal
          1 -> addDeltaAnnotation GHC.AnnVal
          x -> error $ "annotateP.RdrName: too many AnnVal :" ++ showGhc (l,x)
        addDeltaAnnotation GHC.AnnTildehsh
        addDeltaAnnotation GHC.AnnTilde
        addDeltaAnnotation GHC.AnnRarrow
        addDeltaAnnotationLs GHC.AnnBackquote 1
        addDeltaAnnotation GHC.AnnCloseP -- ')'

-- ---------------------------------------------------------------------
{-
instance AnnotateP GHC.Name where
  annotateP l n = do
    addDeltaAnnotationExt l GHC.AnnVal
-}
-- ---------------------------------------------------------------------

instance AnnotateP (GHC.ImportDecl GHC.RdrName) where
 annotateP l (GHC.ImportDecl _msrc (GHC.L ln _) _pkg _src _safe qual _impl as hiding) = do

   -- 'import' maybe_src maybe_safe optqualified maybe_pkg modid maybeas maybeimpspec
   addDeltaAnnotation GHC.AnnImport

   -- "{-# SOURCE" and "#-}"
   addDeltaAnnotation GHC.AnnOpen
   addDeltaAnnotation GHC.AnnClose
   addDeltaAnnotation GHC.AnnSafe
   addDeltaAnnotation GHC.AnnQualified
   addDeltaAnnotation GHC.AnnPackageName

   addDeltaAnnotationExt ln GHC.AnnVal -- modid

   addDeltaAnnotation GHC.AnnAs
   addDeltaAnnotation GHC.AnnVal -- as modid

   case hiding of
     Nothing -> return ()
     Just (_isHiding,lie) -> do
       addDeltaAnnotation GHC.AnnHiding
       annotatePC lie

   -- There may be multiple ';'s
   -- addDeltaAnnotations GHC.AnnSemi

-- ---------------------------------------------------------------------

instance (GenericCon name,GHC.OutputableBndr name,AnnotateP name)
  => AnnotateP (GHC.HsDecl name) where
  annotateP l decl = do
    case decl of
      GHC.TyClD d       -> annotateP l d
      GHC.InstD d       -> annotateP l d
      GHC.DerivD d      -> annotateP l d
      GHC.ValD d        -> annotateP l d
      GHC.SigD d        -> annotateP l d
      GHC.DefD d        -> annotateP l d
      GHC.ForD d        -> annotateP l d
      GHC.WarningD d    -> annotateP l d
      GHC.AnnD d        -> annotateP l d
      GHC.RuleD d       -> annotateP l d
      GHC.VectD d       -> annotateP l d
      GHC.SpliceD d     -> annotateP l d
      GHC.DocD d        -> annotateP l d
      GHC.QuasiQuoteD d -> annotateP l d
      GHC.RoleAnnotD d  -> annotateP l d

-- ---------------------------------------------------------------------

instance (AnnotateP name)
   => AnnotateP (GHC.RoleAnnotDecl name) where
  annotateP l (GHC.RoleAnnotDecl ln mr) = do
    addDeltaAnnotation GHC.AnnType
    addDeltaAnnotation GHC.AnnRole
    annotatePC ln
    mapM_ annotatePC mr

{-
RoleAnnotDecl (Located name) [Located (Maybe Role)]
    AnnKeywordId : AnnType, AnnRole
-}
instance AnnotateP (Maybe GHC.Role) where
  annotateP l _ = addDeltaAnnotationExt l GHC.AnnVal

-- ---------------------------------------------------------------------

instance (AnnotateP name)
   => AnnotateP (GHC.HsQuasiQuote name) where
  annotateP l (GHC.HsQuasiQuote n ss fs) = assert False undefined

-- HsQuasiQuote id SrcSpan FastString
-- ---------------------------------------------------------------------

instance (GenericCon name,GHC.OutputableBndr name,AnnotateP name)
   => AnnotateP (GHC.SpliceDecl name) where
  annotateP l (GHC.SpliceDecl (GHC.L ls (GHC.HsSplice n e)) _flag) = do
    addDeltaAnnotation GHC.AnnOpen -- "$(" or "$$("
    annotatePC e
    addDeltaAnnotation GHC.AnnClose -- ")"

-- ---------------------------------------------------------------------

instance (GenericCon name,GHC.OutputableBndr name,AnnotateP name)
   => AnnotateP (GHC.VectDecl name) where
  annotateP l (GHC.HsVect src ln e) = do
    addDeltaAnnotation GHC.AnnOpen -- "{-# VECTORISE"
    annotatePC ln
    addDeltaAnnotation GHC.AnnEqual
    annotatePC e
    addDeltaAnnotation GHC.AnnClose -- "#-}"

  annotateP l (GHC.HsNoVect src ln) = do
    addDeltaAnnotation GHC.AnnOpen -- "{-# NOVECTORISE"
    annotatePC ln
    addDeltaAnnotation GHC.AnnClose -- "#-}"

  annotateP l (GHC.HsVectTypeIn src b ln mln) = do
    addDeltaAnnotation GHC.AnnOpen -- "{-# VECTORISE" or "{-# VECTORISE SCALAR"
    addDeltaAnnotation GHC.AnnType
    annotatePC ln
    addDeltaAnnotation GHC.AnnEqual
    annotateMaybe mln
    addDeltaAnnotation GHC.AnnClose -- "#-}"

  annotateP l (GHC.HsVectTypeOut {}) = error $ "annotateP.HsVectTypeOut: only valid after type checker"

  annotateP l (GHC.HsVectClassIn src ln) = do
    addDeltaAnnotation GHC.AnnOpen -- "{-# VECTORISE"
    addDeltaAnnotation GHC.AnnClass
    annotatePC ln
    addDeltaAnnotation GHC.AnnClose -- "#-}"

  annotateP l (GHC.HsVectClassOut {}) = error $ "annotateP.HsVectClassOut: only valid after type checker"
  annotateP l (GHC.HsVectInstIn {})   = error $ "annotateP.HsVectInstIn: not supported?"
  annotateP l (GHC.HsVectInstOut {})   = error $ "annotateP.HsVectInstOut: not supported?"

-- ---------------------------------------------------------------------

instance (GenericCon name,GHC.OutputableBndr name,AnnotateP name)
   => AnnotateP (GHC.RuleDecls name) where
   annotateP l (GHC.HsRules src rules) = do
     addDeltaAnnotation GHC.AnnOpen
     mapM_ annotatePC rules
     addDeltaAnnotation GHC.AnnClose

-- ---------------------------------------------------------------------

instance (GenericCon name,GHC.OutputableBndr name,AnnotateP name)
   => AnnotateP (GHC.RuleDecl name) where
  annotateP l (GHC.HsRule ln act bndrs lhs _ rhs _) = do
    annotatePC ln
    -- activation
    addDeltaAnnotation GHC.AnnOpenS -- "["
    addDeltaAnnotation GHC.AnnTilde
    addDeltaAnnotation GHC.AnnVal
    addDeltaAnnotation GHC.AnnCloseS -- "]"

    addDeltaAnnotation GHC.AnnForall
    mapM_ annotatePC bndrs
    addDeltaAnnotation GHC.AnnDot

    annotatePC lhs
    addDeltaAnnotation GHC.AnnEqual
    annotatePC rhs

-- ---------------------------------------------------------------------

instance (GenericCon name,GHC.OutputableBndr name,AnnotateP name)
   => AnnotateP (GHC.RuleBndr name) where
  annotateP l (GHC.RuleBndr ln) = annotatePC ln
  annotateP l (GHC.RuleBndrSig ln (GHC.HsWB thing _ _ _)) = do
    addDeltaAnnotation GHC.AnnOpenP -- "("
    annotatePC ln
    addDeltaAnnotation GHC.AnnDcolon
    annotatePC thing
    addDeltaAnnotation GHC.AnnCloseP -- ")"

-- ---------------------------------------------------------------------

instance (GenericCon name,GHC.OutputableBndr name,AnnotateP name)
   => AnnotateP (GHC.AnnDecl name) where
   annotateP l (GHC.HsAnnotation src prov e) = do
     addDeltaAnnotation GHC.AnnOpen -- "{-# Ann"
     addDeltaAnnotation GHC.AnnType
     addDeltaAnnotation GHC.AnnModule
     case prov of
       (GHC.ValueAnnProvenance n) -> annotatePC n
       (GHC.TypeAnnProvenance n) -> annotatePC n
       (GHC.ModuleAnnProvenance) -> return ()

     annotatePC e
     addDeltaAnnotation GHC.AnnClose

-- ---------------------------------------------------------------------

instance (AnnotateP name)
   => AnnotateP (GHC.WarnDecls name) where
   annotateP l (GHC.Warnings src warns) = do
     addDeltaAnnotation GHC.AnnOpen
     mapM_ annotatePC warns
     addDeltaAnnotation GHC.AnnClose

-- ---------------------------------------------------------------------

instance (AnnotateP name)
   => AnnotateP (GHC.WarnDecl name) where
   annotateP l (GHC.Warning lns txt) = do
     mapM_ annotatePC lns
     addDeltaAnnotation GHC.AnnOpenS -- "["
     case txt of
       GHC.WarningTxt    src ls -> mapM_ annotatePC ls
       GHC.DeprecatedTxt src ls -> mapM_ annotatePC ls
     addDeltaAnnotation GHC.AnnCloseS -- "]"

instance AnnotateP GHC.FastString where
  annotateP l _ = addDeltaAnnotationExt l GHC.AnnVal

-- ---------------------------------------------------------------------

instance (GenericCon name,GHC.OutputableBndr name,AnnotateP name)
   => AnnotateP (GHC.ForeignDecl name) where

  annotateP l (GHC.ForeignImport ln typ _
               (GHC.CImport cconv safety@(GHC.L ll _) mh imp (GHC.L ls src))) = do
    addDeltaAnnotation GHC.AnnForeign
    addDeltaAnnotation GHC.AnnImport
    annotatePC cconv
    if ll == GHC.noSrcSpan
      then return ()
      else annotatePC safety
    -- annotateMaybe mh
    addDeltaAnnotationExt ls GHC.AnnVal
    annotatePC ln
    addDeltaAnnotation GHC.AnnDcolon
    annotatePC typ


  annotateP l (GHC.ForeignExport ln typ _ (GHC.CExport spec (GHC.L ls src))) = do
    addDeltaAnnotation GHC.AnnForeign
    addDeltaAnnotation GHC.AnnExport
    annotatePC spec
    addDeltaAnnotationExt ls GHC.AnnVal
    annotatePC ln
    addDeltaAnnotation GHC.AnnDcolon
    annotatePC typ


-- ---------------------------------------------------------------------

instance (AnnotateP GHC.CExportSpec) where
  annotateP l (GHC.CExportStatic _ cconv) = annotateP l cconv

-- ---------------------------------------------------------------------

instance (AnnotateP GHC.CCallConv) where
  annotateP l _ = addDeltaAnnotationExt l GHC.AnnVal

-- ---------------------------------------------------------------------

instance (AnnotateP GHC.Safety) where
  annotateP l _ = addDeltaAnnotationExt l GHC.AnnVal

-- ---------------------------------------------------------------------

instance (GenericCon name,GHC.OutputableBndr name,AnnotateP name)
   => AnnotateP (GHC.DerivDecl name) where

  annotateP l (GHC.DerivDecl typ mov) = do
    addDeltaAnnotation GHC.AnnDeriving
    addDeltaAnnotation GHC.AnnInstance
    annotateMaybe mov
    annotatePC typ

-- ---------------------------------------------------------------------

instance (GenericCon name,GHC.OutputableBndr name,AnnotateP name)
   => AnnotateP (GHC.DefaultDecl name) where

  annotateP l (GHC.DefaultDecl typs) = do
    addDeltaAnnotation GHC.AnnDefault
    addDeltaAnnotation GHC.AnnOpenP -- '('
    mapM_ annotatePC typs
    addDeltaAnnotation GHC.AnnCloseP -- ')'

-- ---------------------------------------------------------------------

instance (GenericCon name,GHC.OutputableBndr name,AnnotateP name)
   => AnnotateP (GHC.InstDecl name) where

  annotateP l (GHC.ClsInstD      cid) = annotateP l  cid
  annotateP l (GHC.DataFamInstD dfid) = annotateP l dfid
  annotateP l (GHC.TyFamInstD   tfid) = annotateP l tfid

-- ---------------------------------------------------------------------

instance AnnotateP (GHC.OverlapMode) where
  annotateP l _ = do
    addDeltaAnnotation GHC.AnnOpen
    addDeltaAnnotation GHC.AnnClose

-- ---------------------------------------------------------------------

instance (GenericCon name,GHC.OutputableBndr name,AnnotateP name)
   => AnnotateP (GHC.ClsInstDecl name) where

  annotateP l (GHC.ClsInstDecl poly binds sigs tyfams datafams mov) = do
    addDeltaAnnotation GHC.AnnInstance
    annotateMaybe mov
    annotatePC poly
    addDeltaAnnotation GHC.AnnWhere
    addDeltaAnnotation GHC.AnnOpenC -- '{'
    addDeltaAnnotationsInside GHC.AnnSemi

    -- must merge all the rest
    applyListAnnotations (prepareListAnnotation (GHC.bagToList binds)
                       ++ prepareListAnnotation sigs
                       ++ prepareListAnnotation tyfams
                       ++ prepareListAnnotation datafams
                         )

    addDeltaAnnotation GHC.AnnCloseC -- '}'

-- ---------------------------------------------------------------------

instance (GenericCon name,GHC.OutputableBndr name,AnnotateP name)
   => AnnotateP (GHC.TyFamInstDecl name) where

  annotateP l (GHC.TyFamInstDecl eqn _) = do
    addDeltaAnnotation GHC.AnnType
    addDeltaAnnotation GHC.AnnInstance
    annotatePC eqn

-- ---------------------------------------------------------------------

instance (GenericCon name,GHC.OutputableBndr name,AnnotateP name)
   => AnnotateP (GHC.DataFamInstDecl name) where

  annotateP l (GHC.DataFamInstDecl ln (GHC.HsWB pats _ _ _) defn _) = do
    addDeltaAnnotation GHC.AnnData
    addDeltaAnnotation GHC.AnnNewtype
    addDeltaAnnotation GHC.AnnInstance
    annotatePC ln
    mapM_ annotatePC pats
    addDeltaAnnotation GHC.AnnWhere
    addDeltaAnnotation GHC.AnnEqual
    annotateDataDefn l defn

-- ---------------------------------------------------------------------

instance (GenericCon name,GHC.OutputableBndr name,AnnotateP name) =>
                                              AnnotateP (GHC.HsBind name) where
  annotateP l (GHC.FunBind (GHC.L ln n) isInfix (GHC.MG matches _ _ _) _ _ _) = do
    setFunIsInfix isInfix
    mapM_ annotatePC matches

  annotateP l (GHC.PatBind lhs@(GHC.L ll _) grhss@(GHC.GRHSs grhs lb) _typ _fvs _ticks) = do
    annotatePC lhs
    addDeltaAnnotation GHC.AnnEqual
    mapM_ annotatePC grhs
    addDeltaAnnotation GHC.AnnWhere
    annotateHsLocalBinds lb

  annotateP l (GHC.VarBind n rhse _) = do
    -- Note: this bind is introduced by the typechecker
    annotatePC rhse

  annotateP l (GHC.PatSynBind (GHC.PSB ln _fvs args def dir)) = do
    addDeltaAnnotation GHC.AnnPattern
    annotatePC ln
    case args of
      GHC.InfixPatSyn la lb -> do
        annotatePC la
        annotatePC lb
      GHC.PrefixPatSyn ns -> do
        mapM_ annotatePC ns
    addDeltaAnnotation GHC.AnnEqual
    addDeltaAnnotation GHC.AnnLarrow
    annotatePC def
    case dir of
      GHC.Unidirectional           -> return ()
      GHC.ImplicitBidirectional    -> return ()
      GHC.ExplicitBidirectional mg -> annotateMatchGroup mg

    addDeltaAnnotation GHC.AnnWhere
    addDeltaAnnotation GHC.AnnOpenC  -- '{'
    addDeltaAnnotation GHC.AnnCloseC -- '}'

    return ()

-- ---------------------------------------------------------------------

instance (GenericCon name,GHC.OutputableBndr name,AnnotateP name)
    => AnnotateP (GHC.IPBind name) where
  annotateP l (GHC.IPBind en e) = do
    case en of
      Left n -> annotatePC n
      Right i -> error $ "annotateP.IPBind:should not happen"
    addDeltaAnnotation GHC.AnnEqual
    annotatePC e

-- ---------------------------------------------------------------------

instance AnnotateP GHC.HsIPName where
  annotateP l _ = addDeltaAnnotationExt l GHC.AnnVal

-- ---------------------------------------------------------------------

instance (GenericCon name,GHC.OutputableBndr name,AnnotateP name,
                                  AnnotateP body)
  => AnnotateP (GHC.Match name (GHC.Located body)) where

  annotateP l (GHC.Match mln pats _typ grhss@(GHC.GRHSs grhs lb)) = do
    isInfix <- getFunIsInfix
    let
      get_infix Nothing = isInfix
      get_infix (Just (_,f)) = f
    case (get_infix mln,pats) of
      (True,[a,b]) -> do
        annotatePC a
        case mln of
          Nothing -> do
            addDeltaAnnotation GHC.AnnOpen -- possible '`'
            addDeltaAnnotation GHC.AnnFunId
            addDeltaAnnotation GHC.AnnClose -- possible '`'
          Just (n,_) -> annotatePC n
        annotatePC b
      _ -> do
        case mln of
          Nothing -> addDeltaAnnotation GHC.AnnFunId
          Just (n,_) -> annotatePC n
        mapM_ annotatePC pats

    addDeltaAnnotation GHC.AnnEqual
    addDeltaAnnotation GHC.AnnRarrow -- For HsLam

    mapM_ annotatePC grhs

    addDeltaAnnotation GHC.AnnWhere
    addDeltaAnnotation GHC.AnnOpenC -- '{'
    addDeltaAnnotationsInside GHC.AnnSemi
    annotateHsLocalBinds lb
    addDeltaAnnotation GHC.AnnCloseC -- '}'

-- ---------------------------------------------------------------------

-- instance (Typeable name,GHC.OutputableBndr name,AnnotateP name)
--   => AnnotateP (GHC.GRHS name (GHC.LHsExpr name)) where

instance (GenericCon name,GHC.OutputableBndr name,AnnotateP name,
                                  AnnotateP body)
  => AnnotateP (GHC.GRHS name (GHC.Located body)) where
  annotateP l (GHC.GRHS guards expr) = do

    addDeltaAnnotation GHC.AnnVbar
    mapM_ annotatePC guards
    addDeltaAnnotation GHC.AnnEqual
    addDeltaAnnotation GHC.AnnRarrow -- in case alts
    annotatePC expr

-- ---------------------------------------------------------------------

instance (GenericCon name,GHC.OutputableBndr name,AnnotateP name) =>
                                         AnnotateP (GHC.Sig name) where

  annotateP l (GHC.TypeSig lns typ _) = do
    mapM_ annotatePC lns
    addDeltaAnnotation GHC.AnnDcolon
    annotatePC typ

  -- PatSynSig (Located name) (HsExplicitFlag, LHsTyVarBndrs name)
  --           (LHsContext name) (LHsContext name) (LHsType name)
  annotateP l (GHC.PatSynSig ln (_,GHC.HsQTvs _ns bndrs) ctx1 ctx2 typ) = do
    addDeltaAnnotation GHC.AnnPattern
    annotatePC ln
    addDeltaAnnotation GHC.AnnDcolon

    -- Note: The 'forall' bndrs '.' may occur multiple times
    addDeltaAnnotation GHC.AnnForall
    mapM_ annotatePC bndrs
    addDeltaAnnotation GHC.AnnDot

    annotatePC ctx1
    addDeltaAnnotationLs GHC.AnnDarrow 0
    annotatePC ctx2
    addDeltaAnnotationLs GHC.AnnDarrow 1
    annotatePC typ


  annotateP l (GHC.GenericSig ns typ) = do
    addDeltaAnnotation GHC.AnnDefault
    mapM_ annotatePC ns
    addDeltaAnnotation GHC.AnnDcolon
    annotatePC typ

  annotateP l (GHC.IdSig _) = return ()

  -- FixSig (FixitySig name)
  annotateP l (GHC.FixSig (GHC.FixitySig lns (GHC.Fixity v fdir))) = do
    addDeltaAnnotation GHC.AnnInfix
    addDeltaAnnotation GHC.AnnVal
    mapM_ annotatePC lns

  -- InlineSig (Located name) InlinePragma
  -- '{-# INLINE' activation qvar '#-}'
  annotateP l (GHC.InlineSig ln inl) = do
    addDeltaAnnotation GHC.AnnOpen -- '{-# INLINE'
    addDeltaAnnotation GHC.AnnOpenS -- '['
    addDeltaAnnotation  GHC.AnnTilde -- ~
    addDeltaAnnotation  GHC.AnnVal   -- e.g. 34
    addDeltaAnnotation GHC.AnnCloseS  -- ']'
    annotatePC ln
    addDeltaAnnotation GHC.AnnClose -- '#-}'


  annotateP l (GHC.SpecSig ln typs inl) = do
    addDeltaAnnotation GHC.AnnOpen -- '{-# SPECIALISE'
    addDeltaAnnotation GHC.AnnOpenS --  '['
    addDeltaAnnotation GHC.AnnTilde -- ~
    addDeltaAnnotation GHC.AnnVal   -- e.g. 34

    addDeltaAnnotation GHC.AnnCloseS -- ']'
    annotatePC ln
    addDeltaAnnotation GHC.AnnDcolon -- '::'
    mapM_ annotatePC typs
    addDeltaAnnotation GHC.AnnClose -- '#-}'


  -- '{-# SPECIALISE' 'instance' inst_type '#-}'
  annotateP l (GHC.SpecInstSig _ typ) = do
    addDeltaAnnotation GHC.AnnOpen -- '{-# SPECIALISE'
    addDeltaAnnotation GHC.AnnInstance
    annotatePC typ
    addDeltaAnnotation GHC.AnnClose -- '#-}'


  -- MinimalSig (BooleanFormula (Located name))
  annotateP l (GHC.MinimalSig _ formula) = do
    addDeltaAnnotation GHC.AnnOpen -- '{-# MINIMAL'
    annotateBooleanFormula formula
    addDeltaAnnotation GHC.AnnClose -- '#-}'


-- ---------------------------------------------------------------------

-- data BooleanFormula a = Var a | And [BooleanFormula a] | Or [BooleanFormula a]
--  deriving (Eq, Data, Typeable, Functor, Foldable, Traversable)

annotateBooleanFormula :: GHC.BooleanFormula (GHC.Located name) -> AP ()
annotateBooleanFormula = assert False undefined

-- ---------------------------------------------------------------------

instance (GenericCon name,GHC.OutputableBndr name,AnnotateP name) =>
                     AnnotateP (GHC.HsTyVarBndr name) where
  annotateP l (GHC.UserTyVar n) = do
    addDeltaAnnotationExt l GHC.AnnVal

  annotateP l (GHC.KindedTyVar n ty) = do
    addDeltaAnnotation GHC.AnnOpenP -- '('
    annotatePC n
    addDeltaAnnotation GHC.AnnDcolon -- '::'
    annotatePC ty
    addDeltaAnnotation GHC.AnnCloseP -- '('

-- ---------------------------------------------------------------------

instance (GenericCon name,GHC.OutputableBndr name,AnnotateP name)
   => AnnotateP (GHC.HsType name) where

  annotateP l (GHC.HsForAllTy f mwc (GHC.HsQTvs kvs tvs) ctx@(GHC.L lc ctxs) typ) = do
    addDeltaAnnotation GHC.AnnOpenP -- "("
    addDeltaAnnotation GHC.AnnForall
    mapM_ annotatePC tvs
    addDeltaAnnotation GHC.AnnDot

    case mwc of
      Nothing -> if lc /= GHC.noSrcSpan then annotatePC ctx else return ()
      Just lwc -> annotatePC (GHC.L lc (GHC.sortLocated ((GHC.L lwc GHC.HsWildcardTy):ctxs)))

    addDeltaAnnotation GHC.AnnDarrow
    annotatePC typ
    addDeltaAnnotation GHC.AnnCloseP -- ")"

  annotateP l (GHC.HsTyVar n) = do
    addDeltaAnnotation GHC.AnnDcolon -- for HsKind, alias for HsType
    annotateP l n

  annotateP l (GHC.HsAppTy t1 t2) = do
    addDeltaAnnotation GHC.AnnDcolon -- for HsKind, alias for HsType
    annotatePC t1
    annotatePC t2

  annotateP l (GHC.HsFunTy t1 t2) = do
    addDeltaAnnotation GHC.AnnDcolon -- for HsKind, alias for HsType
    annotatePC t1
    addDeltaAnnotation GHC.AnnRarrow
    annotatePC t2

  annotateP l (GHC.HsListTy t) = do
    addDeltaAnnotation GHC.AnnDcolon -- for HsKind, alias for HsType
    addDeltaAnnotation GHC.AnnOpenS -- '['
    annotatePC t
    addDeltaAnnotation GHC.AnnCloseS -- ']'

  annotateP l (GHC.HsPArrTy t) = do
    addDeltaAnnotation GHC.AnnOpen  -- '[:'
    annotatePC t
    addDeltaAnnotation GHC.AnnClose -- ':]'

  annotateP l (GHC.HsTupleTy tt ts) = do
    addDeltaAnnotation GHC.AnnDcolon -- for HsKind, alias for HsType
    addDeltaAnnotation GHC.AnnOpen  -- '(#'
    addDeltaAnnotation GHC.AnnOpenP  -- '('
    mapM_ annotatePC ts
    addDeltaAnnotation GHC.AnnCloseP -- ')'
    addDeltaAnnotation GHC.AnnClose --  '#)'

  annotateP l (GHC.HsOpTy t1 (_,lo) t2) = do
    annotatePC t1
    annotatePC lo
    annotatePC t2

  annotateP l (GHC.HsParTy t) = do
    addDeltaAnnotation GHC.AnnDcolon -- for HsKind, alias for HsType
    addDeltaAnnotation GHC.AnnOpenP  -- '('
    annotatePC t
    addDeltaAnnotation GHC.AnnCloseP -- ')'

  annotateP l (GHC.HsIParamTy n t) = do
    addDeltaAnnotation GHC.AnnVal
    addDeltaAnnotation GHC.AnnDcolon
    annotatePC t

  annotateP l (GHC.HsEqTy t1 t2) = do
    annotatePC t1
    addDeltaAnnotation GHC.AnnTilde
    annotatePC t2

  annotateP l (GHC.HsKindSig t k) = do
    addDeltaAnnotation GHC.AnnOpenP  -- '('
    annotatePC t
    addDeltaAnnotation GHC.AnnDcolon -- '::'
    annotatePC k
    addDeltaAnnotation GHC.AnnCloseP -- ')'

  -- HsQuasiQuoteTy (HsQuasiQuote name)
  annotateP l (GHC.HsQuasiQuoteTy qq) = do
    addDeltaAnnotationExt l GHC.AnnVal

  -- HsSpliceTy (HsSplice name) (PostTc name Kind)
  annotateP l (GHC.HsSpliceTy (GHC.HsSplice _is e) _) = do
    addDeltaAnnotation GHC.AnnOpen  -- '$('
    annotatePC e
    addDeltaAnnotation GHC.AnnClose -- ')'

  annotateP l (GHC.HsDocTy t ds) = do
    annotatePC t
    annotatePC ds

  annotateP l (GHC.HsBangTy _b t) = do
    addDeltaAnnotation GHC.AnnOpen  -- '{-# UNPACK' or '{-# NOUNPACK'
    addDeltaAnnotation GHC.AnnClose -- '#-}'
    addDeltaAnnotation GHC.AnnBang  -- '!'
    annotatePC t

  -- HsRecTy [LConDeclField name]
  annotateP l (GHC.HsRecTy cons) = do
    addDeltaAnnotation GHC.AnnOpenC  -- '{'
    mapM_ annotatePC cons
    addDeltaAnnotation GHC.AnnCloseC -- '}'

  -- HsCoreTy Type
  annotateP l (GHC.HsCoreTy _t) = return ()

  annotateP l (GHC.HsExplicitListTy _ ts) = do
    -- TODO: what about SIMPLEQUOTE?
    addDeltaAnnotation GHC.AnnOpen  -- "'["
    mapM_ annotatePC ts
    addDeltaAnnotation GHC.AnnCloseS -- ']'

  annotateP l (GHC.HsExplicitTupleTy _ ts) = do
    addDeltaAnnotation GHC.AnnOpen  -- "'("
    mapM_ annotatePC ts
    addDeltaAnnotation GHC.AnnClose -- ')'

  -- HsTyLit HsTyLit
  annotateP l (GHC.HsTyLit tl) = do
    addDeltaAnnotationExt l GHC.AnnVal

  -- HsWrapTy HsTyWrapper (HsType name)
  annotateP l (GHC.HsWrapTy _ _) = return ()

  annotateP l (GHC.HsWildcardTy) = do
    addDeltaAnnotationExt l GHC.AnnVal
    addDeltaAnnotation GHC.AnnDarrow -- if only part of a partial type signature context

  annotateP l (GHC.HsNamedWildcardTy n) = do
    addDeltaAnnotationExt l GHC.AnnVal

-- ---------------------------------------------------------------------

instance (GenericCon name,GHC.OutputableBndr name,AnnotateP name) =>
                             AnnotateP (GHC.ConDeclField name) where
  annotateP l (GHC.ConDeclField ns ty mdoc) = do
    mapM_ annotatePC ns
    addDeltaAnnotation GHC.AnnDcolon
    annotatePC ty
    annotateMaybe mdoc

-- ---------------------------------------------------------------------

instance AnnotateP GHC.HsDocString where
  annotateP l (GHC.HsDocString _s) = do
    addDeltaAnnotationExt l GHC.AnnVal

-- ---------------------------------------------------------------------

instance (GenericCon name,AnnotateP name,GHC.OutputableBndr name)
  => AnnotateP (GHC.Pat name) where
  annotateP l (GHC.WildPat _) = addDeltaAnnotationExt l GHC.AnnVal
  annotateP l (GHC.VarPat _)  = addDeltaAnnotationExt l GHC.AnnVal
  annotateP l (GHC.LazyPat p) = do
    addDeltaAnnotation GHC.AnnTilde
    annotatePC p

  annotateP l (GHC.AsPat ln p) = do
    annotatePC ln
    addDeltaAnnotation GHC.AnnAt
    annotatePC p

  annotateP l (GHC.ParPat p) = do
    addDeltaAnnotation GHC.AnnOpenP
    annotatePC p
    addDeltaAnnotation GHC.AnnCloseP

  annotateP l (GHC.BangPat p) = do
    addDeltaAnnotation GHC.AnnBang
    annotatePC p

  annotateP l (GHC.ListPat ps _ _) = do
    addDeltaAnnotation GHC.AnnOpenS
    mapM_ annotatePC ps
    addDeltaAnnotation GHC.AnnCloseS

  annotateP l (GHC.TuplePat ps _ _) = do
    addDeltaAnnotation GHC.AnnOpen
    addDeltaAnnotation GHC.AnnOpenP
    mapM_ annotatePC ps
    addDeltaAnnotation GHC.AnnCloseP
    addDeltaAnnotation GHC.AnnClose

  annotateP l (GHC.PArrPat ps _) = do
    addDeltaAnnotation GHC.AnnOpen
    mapM_ annotatePC ps
    addDeltaAnnotation GHC.AnnClose

  annotateP l (GHC.ConPatIn n dets) = do
    annotateHsConPatDetails n dets

  annotateP l (GHC.ConPatOut {}) = return ()

  -- ViewPat (LHsExpr id) (LPat id) (PostTc id Type)
  annotateP l (GHC.ViewPat e pat _) = do
    annotatePC e
    addDeltaAnnotation GHC.AnnRarrow
    annotatePC pat

  -- SplicePat (HsSplice id)
  annotateP l (GHC.SplicePat (GHC.HsSplice _ e)) = do
    addDeltaAnnotation GHC.AnnOpen -- '$('
    annotatePC e
    addDeltaAnnotation GHC.AnnClose -- ')'

  -- QuasiQuotePat (HsQuasiQuote id)
  annotateP l (GHC.QuasiQuotePat (GHC.HsQuasiQuote _ _ _)) = do
    addDeltaAnnotationExt l GHC.AnnVal

  -- LitPat HsLit
  annotateP l (GHC.LitPat lp) = addDeltaAnnotationExt l GHC.AnnVal

  -- NPat (HsOverLit id) (Maybe (SyntaxExpr id)) (SyntaxExpr id)
  annotateP l (GHC.NPat ol _ _) = do
    addDeltaAnnotation GHC.AnnMinus
    -- addDeltaAnnotationExt l GHC.AnnVal
    annotatePC ol

  -- NPlusKPat (Located id) (HsOverLit id) (SyntaxExpr id) (SyntaxExpr id)
  annotateP l (GHC.NPlusKPat ln ol _ _) = do
    annotatePC ln
    addDeltaAnnotation GHC.AnnVal -- "+"
    annotatePC ol

  annotateP l (GHC.SigPatIn pat ty) = do
    annotatePC pat
    addDeltaAnnotation GHC.AnnDcolon
    annotateP l ty

  annotateP l (GHC.SigPatOut {}) = return ()

  -- CoPat HsWrapper (Pat id) Type
  annotateP l (GHC.CoPat {}) = return ()

-- ---------------------------------------------------------------------

annotateHsConPatDetails :: (GenericCon name,GHC.OutputableBndr name,AnnotateP name)
                      => GHC.Located name -> GHC.HsConPatDetails name -> AP ()
annotateHsConPatDetails ln dets = do
  case dets of
    GHC.PrefixCon args -> do
      annotatePC ln
      mapM_ annotatePC args
    GHC.RecCon (GHC.HsRecFields fs _) -> do
      annotatePC ln
      addDeltaAnnotation GHC.AnnOpenC -- '{'
      mapM_ annotatePC fs
      addDeltaAnnotation GHC.AnnDotdot
      addDeltaAnnotation GHC.AnnCloseC -- '}'
    GHC.InfixCon a1 a2 -> do
      annotatePC a1
      annotatePC ln
      annotatePC a2

annotateHsConDeclDetails :: (GenericCon name,GHC.OutputableBndr name,AnnotateP name)
                    =>  [GHC.Located name] -> GHC.HsConDeclDetails name -> AP ()
annotateHsConDeclDetails lns dets = do
  case dets of
    GHC.PrefixCon args -> mapM_ annotatePC args
    GHC.RecCon fs -> do
      addDeltaAnnotation GHC.AnnOpenC
      annotatePC fs
      addDeltaAnnotation GHC.AnnCloseC
    GHC.InfixCon a1 a2 -> do
      annotatePC a1
      mapM_ annotatePC lns
      annotatePC a2

-- ---------------------------------------------------------------------

instance (GenericCon name,GHC.OutputableBndr name,AnnotateP name)
   => AnnotateP [GHC.LConDeclField name] where
  annotateP l fs = do
       addDeltaAnnotation GHC.AnnOpenC -- '{'
       mapM_ annotatePC fs
       addDeltaAnnotation GHC.AnnDotdot
       addDeltaAnnotation GHC.AnnCloseC -- '}'

-- ---------------------------------------------------------------------

instance (GenericCon name) => AnnotateP (GHC.HsOverLit name) where
  annotateP l ol = addDeltaAnnotationExt l GHC.AnnVal

-- ---------------------------------------------------------------------

instance (GenericCon name,AnnotateP arg)
    => AnnotateP (GHC.HsWithBndrs name (GHC.Located arg)) where
  annotateP l (GHC.HsWB thing _ _ _) = annotatePC thing

-- ---------------------------------------------------------------------

instance (GenericCon name,GHC.OutputableBndr name,AnnotateP name,AnnotateP body) =>
                            AnnotateP (GHC.Stmt name (GHC.Located body)) where

  annotateP l (GHC.LastStmt body _) = annotatePC body

  annotateP l (GHC.BindStmt pat body _ _) = do
    annotatePC pat
    addDeltaAnnotation GHC.AnnLarrow
    annotatePC body
    addDeltaAnnotation GHC.AnnVbar -- possible in list comprehension

  annotateP l (GHC.BodyStmt body _ _ _) = do
    annotatePC body

  annotateP l (GHC.LetStmt lb) = do
    addDeltaAnnotation GHC.AnnLet
    addDeltaAnnotation GHC.AnnOpenC -- '{'
    annotateHsLocalBinds lb
    addDeltaAnnotation GHC.AnnCloseC -- '}'

  annotateP l (GHC.ParStmt pbs _ _) = do
    mapM_ annotateParStmtBlock pbs

  annotateP l (GHC.TransStmt form stmts _b using by _ _ _) = do
    mapM_ annotatePC stmts
    case form of
      GHC.ThenForm -> do
        addDeltaAnnotation GHC.AnnThen
        annotatePC using
        addDeltaAnnotation GHC.AnnBy
        case by of
          Just b -> annotatePC b
          Nothing -> return ()
      GHC.GroupForm -> do
        addDeltaAnnotation GHC.AnnThen
        addDeltaAnnotation GHC.AnnGroup
        addDeltaAnnotation GHC.AnnBy
        case by of
          Just b -> annotatePC b
          Nothing -> return ()
        addDeltaAnnotation GHC.AnnUsing
        annotatePC using

  annotateP l (GHC.RecStmt stmts _ _ _ _ _ _ _ _) = do
    addDeltaAnnotation GHC.AnnRec
    addDeltaAnnotation GHC.AnnOpenC
    addDeltaAnnotationsInside GHC.AnnSemi
    mapM_ annotatePC stmts
    addDeltaAnnotation GHC.AnnCloseC

-- ---------------------------------------------------------------------

annotateParStmtBlock :: (GenericCon name,GHC.OutputableBndr name, AnnotateP name) =>  GHC.ParStmtBlock name name -> AP ()
annotateParStmtBlock (GHC.ParStmtBlock stmts ns _) = do
  mapM_ annotatePC stmts

-- ---------------------------------------------------------------------

annotateHsLocalBinds :: (GenericCon name,GHC.OutputableBndr name,AnnotateP name)
                     => (GHC.HsLocalBinds name) -> AP ()
annotateHsLocalBinds (GHC.HsValBinds (GHC.ValBindsIn binds sigs)) = do
    applyListAnnotations (prepareListAnnotation (GHC.bagToList binds)
                       ++ prepareListAnnotation sigs
                         )
annotateHsLocalBinds (GHC.HsValBinds (GHC.ValBindsOut {}))
   = error $ "annotateHsLocalBinds: only valid after type checking"

annotateHsLocalBinds (GHC.HsIPBinds (GHC.IPBinds binds _)) = mapM_ annotatePC binds
annotateHsLocalBinds (GHC.EmptyLocalBinds) = return ()

-- ---------------------------------------------------------------------

-- annotateMatchGroup :: (Typeable name,GHC.OutputableBndr name,AnnotateP name)
--                    =>   (GHC.MatchGroup name (GHC.LHsExpr name))
--                    -> AP ()
annotateMatchGroup :: (GenericCon name,GHC.OutputableBndr name,AnnotateP name,
                                               AnnotateP body)
                   =>   (GHC.MatchGroup name (GHC.Located body))
                   -> AP ()
annotateMatchGroup (GHC.MG matches _ _ _)
  = mapM_ annotatePC matches

-- ---------------------------------------------------------------------
{-
annotateCmdMatchGroup :: (Typeable name,GHC.OutputableBndr name,AnnotateP name) =>
                      (GHC.MatchGroup name (GHC.LHsCmd name))
                   -> AP ()
annotateCmdMatchGroup (GHC.MG matches _ _ _)
  = mapM_ annotatePC matches
-}
-- ---------------------------------------------------------------------

instance (GenericCon name,GHC.OutputableBndr name,AnnotateP name) =>
                               AnnotateP (GHC.HsExpr name) where
  annotateP l (GHC.HsVar n)           = annotateP l n
  annotateP l (GHC.HsIPVar _)         = addDeltaAnnotationExt l GHC.AnnVal
  annotateP l (GHC.HsOverLit ov)      = addDeltaAnnotationExt l GHC.AnnVal
  annotateP l (GHC.HsLit _)           = addDeltaAnnotationExt l GHC.AnnVal

  annotateP l (GHC.HsLam match)       = do
    addDeltaAnnotation GHC.AnnLam
    annotateMatchGroup match

  annotateP l (GHC.HsLamCase _ match) = annotateMatchGroup match

  annotateP l (GHC.HsApp e1 e2) = do
    annotatePC e1
    annotatePC e2

  annotateP l (GHC.OpApp e1 e2 _ e3) = do
    annotatePC e1
    annotatePC e2
    annotatePC e3

  annotateP l (GHC.NegApp e _) = do
    addDeltaAnnotation GHC.AnnMinus
    annotatePC e

  annotateP l (GHC.HsPar e) = do
    addDeltaAnnotation GHC.AnnOpenP -- '('
    annotatePC e
    addDeltaAnnotation GHC.AnnCloseP -- ')'

  annotateP l (GHC.SectionL e1 e2) = do
    annotatePC e1
    annotatePC e2

  annotateP l (GHC.SectionR e1 e2) = do
    annotatePC e1
    annotatePC e2

  annotateP l (GHC.ExplicitTuple args boxity) = do
    addDeltaAnnotation GHC.AnnOpen
    addDeltaAnnotation GHC.AnnOpenP
    mapM_ annotatePC args
    addDeltaAnnotation GHC.AnnCloseP
    addDeltaAnnotation GHC.AnnClose

  annotateP l (GHC.HsCase e1 matches) = do
    addDeltaAnnotation GHC.AnnCase
    annotatePC e1
    addDeltaAnnotation GHC.AnnOf
    addDeltaAnnotation GHC.AnnOpenC
    addDeltaAnnotationsInside GHC.AnnSemi
    annotateMatchGroup matches
    addDeltaAnnotation GHC.AnnCloseC

  annotateP l (GHC.HsIf _ e1 e2 e3) = do
    addDeltaAnnotation GHC.AnnIf
    annotatePC e1
    addDeltaAnnotationLs GHC.AnnSemi 0
    addDeltaAnnotation GHC.AnnThen
    annotatePC e2
    addDeltaAnnotationLs GHC.AnnSemi 1
    addDeltaAnnotation GHC.AnnElse
    annotatePC e3

  annotateP l (GHC.HsMultiIf _ rhs) = do
    addDeltaAnnotation GHC.AnnIf
    mapM_ annotatePC rhs

  annotateP l (GHC.HsLet binds e) = do
    addDeltaAnnotation GHC.AnnLet
    addDeltaAnnotation GHC.AnnOpenC
    addDeltaAnnotationsInside GHC.AnnSemi
    annotateHsLocalBinds binds
    addDeltaAnnotation GHC.AnnCloseC
    addDeltaAnnotation GHC.AnnIn
    annotatePC e

  annotateP l (GHC.HsDo cts es _) = do
    addDeltaAnnotation GHC.AnnDo
    addDeltaAnnotation GHC.AnnOpen
    addDeltaAnnotation GHC.AnnOpenS
    addDeltaAnnotation GHC.AnnOpenC
    addDeltaAnnotationsInside GHC.AnnSemi
    if isListComp cts
      then do
        annotatePC (last es)
        addDeltaAnnotation GHC.AnnVbar
        mapM_ annotatePC (init es)
      else do
        mapM_ annotatePC es
    addDeltaAnnotation GHC.AnnCloseS
    addDeltaAnnotation GHC.AnnCloseC
    addDeltaAnnotation GHC.AnnClose

  annotateP l (GHC.ExplicitList _ _ es) = do
    addDeltaAnnotation GHC.AnnOpenS
    mapM_ annotatePC es
    addDeltaAnnotation GHC.AnnCloseS

  annotateP l (GHC.ExplicitPArr _ es)   = do
    addDeltaAnnotation GHC.AnnOpen
    mapM_ annotatePC es
    addDeltaAnnotation GHC.AnnClose

  annotateP l (GHC.RecordCon n _ (GHC.HsRecFields fs _)) = do
    annotatePC n
    addDeltaAnnotation GHC.AnnOpenC
    addDeltaAnnotation GHC.AnnDotdot
    mapM_ annotatePC fs
    addDeltaAnnotation GHC.AnnCloseC

  annotateP l (GHC.RecordUpd e (GHC.HsRecFields fs _) cons _ _) = do
    annotatePC e
    addDeltaAnnotation GHC.AnnOpenC
    addDeltaAnnotation GHC.AnnDotdot
    mapM_ annotatePC fs
    addDeltaAnnotation GHC.AnnCloseC

  annotateP l (GHC.ExprWithTySig e typ _) = do
    annotatePC e
    addDeltaAnnotation GHC.AnnDcolon
    annotatePC typ

{-
  annotateP l (GHC.ExprWithTySigOut e typ) = do
    annotatePC e
    addDeltaAnnotation GHC.AnnDcolon
    annotatePC typ
-}

  annotateP l (GHC.ArithSeq _ _ seqInfo) = do
    addDeltaAnnotation GHC.AnnOpenS -- '['
    case seqInfo of
        GHC.From e -> do
          annotatePC e
          addDeltaAnnotation GHC.AnnDotdot
        GHC.FromTo e1 e2 -> do
          annotatePC e1
          addDeltaAnnotation GHC.AnnDotdot
          annotatePC e2
        GHC.FromThen e1 e2 -> do
          annotatePC e1
          addDeltaAnnotation GHC.AnnComma
          annotatePC e2
          addDeltaAnnotation GHC.AnnDotdot
        GHC.FromThenTo e1 e2 e3 -> do
          annotatePC e1
          addDeltaAnnotation GHC.AnnComma
          annotatePC e2
          addDeltaAnnotation GHC.AnnDotdot
          annotatePC e3
    addDeltaAnnotation GHC.AnnCloseS -- ']'

  annotateP l (GHC.PArrSeq _ seqInfo) = do
    addDeltaAnnotation GHC.AnnOpen -- '[:'
    case seqInfo of
        GHC.From e -> do
          annotatePC e
          addDeltaAnnotation GHC.AnnDotdot
        GHC.FromTo e1 e2 -> do
          annotatePC e1
          addDeltaAnnotation GHC.AnnDotdot
          annotatePC e2
        GHC.FromThen e1 e2 -> do
          annotatePC e1
          addDeltaAnnotation GHC.AnnComma
          annotatePC e2
          addDeltaAnnotation GHC.AnnDotdot
        GHC.FromThenTo e1 e2 e3 -> do
          annotatePC e1
          addDeltaAnnotation GHC.AnnComma
          annotatePC e2
          addDeltaAnnotation GHC.AnnDotdot
          annotatePC e3
    addDeltaAnnotation GHC.AnnClose -- ':]'

  annotateP l (GHC.HsSCC _ csFStr e) = do
    addDeltaAnnotation GHC.AnnOpen -- '{-# SCC'
    addDeltaAnnotation GHC.AnnVal
    addDeltaAnnotation GHC.AnnValStr
    addDeltaAnnotation GHC.AnnClose -- '#-}'
    annotatePC e

  annotateP l (GHC.HsCoreAnn _ csFStr e) = do
    addDeltaAnnotation GHC.AnnOpen -- '{-# CORE'
    addDeltaAnnotation GHC.AnnVal
    addDeltaAnnotation GHC.AnnClose -- '#-}'
    annotatePC e

  annotateP l (GHC.HsBracket (GHC.VarBr _ _)) = do
    addDeltaAnnotationExt l GHC.AnnVal
  annotateP l (GHC.HsBracket (GHC.DecBrL ds)) = do
    addDeltaAnnotation GHC.AnnOpen
    addDeltaAnnotation GHC.AnnOpenC
    mapM_ annotatePC ds
    addDeltaAnnotation GHC.AnnCloseC
    addDeltaAnnotation GHC.AnnClose
  annotateP l (GHC.HsBracket (GHC.ExpBr e)) = do
    addDeltaAnnotation GHC.AnnOpen
    annotatePC e
    addDeltaAnnotation GHC.AnnClose
  annotateP l (GHC.HsBracket (GHC.TExpBr e)) = do
    addDeltaAnnotation GHC.AnnOpen
    annotatePC e
    addDeltaAnnotation GHC.AnnClose
  annotateP l (GHC.HsBracket (GHC.TypBr e)) = do
    addDeltaAnnotation GHC.AnnOpen
    annotatePC e
    addDeltaAnnotation GHC.AnnClose
  annotateP l (GHC.HsBracket (GHC.PatBr e)) = do
    addDeltaAnnotation GHC.AnnOpen
    annotatePC e
    addDeltaAnnotation GHC.AnnClose

  annotateP l (GHC.HsRnBracketOut _ _) = return ()
  annotateP l (GHC.HsTcBracketOut _ _) = return ()

  annotateP l (GHC.HsSpliceE typed (GHC.HsSplice _ e)) = do
    addDeltaAnnotation GHC.AnnOpen -- possible '$('
    annotatePC e
    addDeltaAnnotation GHC.AnnClose -- possible ')'

  annotateP l (GHC.HsQuasiQuoteE (GHC.HsQuasiQuote _ _ _)) = do
    addDeltaAnnotationExt l GHC.AnnVal

  annotateP l (GHC.HsProc p c) = do
    addDeltaAnnotation GHC.AnnProc
    annotatePC p
    addDeltaAnnotation GHC.AnnRarrow
    annotatePC c

  annotateP l (GHC.HsStatic e) = do
    addDeltaAnnotation GHC.AnnStatic
    annotatePC e

  annotateP l (GHC.HsArrApp e1 e2 _ _ _) = do
    annotatePC e1
    -- only one of the next 4 will be resent
    addDeltaAnnotation GHC.Annlarrowtail
    addDeltaAnnotation GHC.Annrarrowtail
    addDeltaAnnotation GHC.AnnLarrowtail
    addDeltaAnnotation GHC.AnnRarrowtail

    annotatePC e1

  annotateP l (GHC.HsArrForm e _ cs) = do
    addDeltaAnnotation GHC.AnnOpen -- '(|'
    annotatePC e
    mapM_ annotatePC cs
    addDeltaAnnotation GHC.AnnClose -- '|)'

  annotateP l (GHC.HsTick _ _) = return ()
  annotateP l (GHC.HsBinTick _ _ _) = return ()

  annotateP l (GHC.HsTickPragma _ (str,(v1,v2),(v3,v4)) e) = do
    -- '{-# GENERATED' STRING INTEGER ':' INTEGER '-' INTEGER ':' INTEGER '#-}'
    addDeltaAnnotation   GHC.AnnOpen     -- '{-# GENERATED'
    addDeltaAnnotationLs GHC.AnnVal   0 -- STRING
    addDeltaAnnotationLs GHC.AnnVal   1 -- INTEGER
    addDeltaAnnotationLs GHC.AnnColon 0 -- ':'
    addDeltaAnnotationLs GHC.AnnVal   2 -- INTEGER
    addDeltaAnnotation   GHC.AnnMinus   -- '-'
    addDeltaAnnotationLs GHC.AnnVal   3 -- INTEGER
    addDeltaAnnotationLs GHC.AnnColon 1 -- ':'
    addDeltaAnnotationLs GHC.AnnVal   4 -- INTEGER
    addDeltaAnnotation   GHC.AnnClose   -- '#-}'
    annotatePC e

  annotateP l (GHC.EWildPat) = do
    addDeltaAnnotationExt l GHC.AnnVal

  annotateP l (GHC.EAsPat ln e) = do
    annotatePC ln
    addDeltaAnnotation GHC.AnnAt
    annotatePC e

  annotateP l (GHC.EViewPat e1 e2) = do
    annotatePC e1
    addDeltaAnnotation GHC.AnnRarrow
    annotatePC e2

  annotateP l (GHC.ELazyPat e) = do
    addDeltaAnnotation GHC.AnnTilde
    annotatePC e

  annotateP l (GHC.HsType ty) = annotatePC ty

  annotateP l (GHC.HsWrap _ _) = return ()
  annotateP l (GHC.HsUnboundVar _) = return ()


-- ---------------------------------------------------------------------

instance (GenericCon name,GHC.OutputableBndr name,AnnotateP name) =>
                             AnnotateP (GHC.HsTupArg name) where
  annotateP l (GHC.Present e@(GHC.L le _)) = do
    annotatePC e

  annotateP l (GHC.Missing _) = do
    addDeltaAnnotation GHC.AnnComma
    return ()

-- ---------------------------------------------------------------------

instance (GenericCon name,GHC.OutputableBndr name,AnnotateP name)
  => AnnotateP (GHC.HsCmdTop name) where
  annotateP l (GHC.HsCmdTop cmd _ _ _) = annotatePC cmd

instance (GenericCon name,GHC.OutputableBndr name,AnnotateP name)
   => AnnotateP (GHC.HsCmd name) where
  annotateP l (GHC.HsCmdArrApp e1 e2 _ _ _) = do
    annotatePC e1
    -- only one of the next 4 will be resent
    addDeltaAnnotation GHC.Annlarrowtail
    addDeltaAnnotation GHC.Annrarrowtail
    addDeltaAnnotation GHC.AnnLarrowtail
    addDeltaAnnotation GHC.AnnRarrowtail

    annotatePC e2

  annotateP l (GHC.HsCmdArrForm e mf cs) = do
    addDeltaAnnotation GHC.AnnOpen -- '(|'
    annotatePC e
    mapM_ annotatePC cs
    addDeltaAnnotation GHC.AnnClose -- '|)'

  annotateP l (GHC.HsCmdApp e1 e2) = do
    annotatePC e1
    annotatePC e2

  annotateP l (GHC.HsCmdLam match) = do
    addDeltaAnnotation GHC.AnnLam
    annotateMatchGroup match

  annotateP l (GHC.HsCmdPar e) = do
    addDeltaAnnotation GHC.AnnOpenP -- '('
    annotatePC e
    addDeltaAnnotation GHC.AnnCloseP -- ')'

  annotateP l (GHC.HsCmdCase e1 matches) = do
    addDeltaAnnotation GHC.AnnCase
    annotatePC e1
    addDeltaAnnotation GHC.AnnOf
    addDeltaAnnotation GHC.AnnOpenC
    annotateMatchGroup matches
    addDeltaAnnotation GHC.AnnCloseC

  annotateP l (GHC.HsCmdIf _ e1 e2 e3) = do
    addDeltaAnnotation GHC.AnnIf
    annotatePC e1
    addDeltaAnnotationLs GHC.AnnSemi 0
    addDeltaAnnotation GHC.AnnThen
    annotatePC e2
    addDeltaAnnotationLs GHC.AnnSemi 1
    addDeltaAnnotation GHC.AnnElse
    annotatePC e3

  annotateP l (GHC.HsCmdLet binds e) = do
    addDeltaAnnotation GHC.AnnLet
    addDeltaAnnotation GHC.AnnOpenC
    annotateHsLocalBinds binds
    addDeltaAnnotation GHC.AnnCloseC
    addDeltaAnnotation GHC.AnnIn
    annotatePC e

  annotateP l (GHC.HsCmdDo es _) = do
    addDeltaAnnotation GHC.AnnDo
    addDeltaAnnotation GHC.AnnOpenC
    mapM_ annotatePC es
    addDeltaAnnotation GHC.AnnCloseC

  annotateP l (GHC.HsCmdCast {}) = error $ "annotateP.HsCmdCast: only valid after type checker"


-- ---------------------------------------------------------------------

instance (GenericCon name,GHC.OutputableBndr name,AnnotateP name)
     => AnnotateP (GHC.TyClDecl name) where

  annotateP l (GHC.FamDecl famdecl) = annotateP l famdecl

  annotateP l (GHC.SynDecl ln (GHC.HsQTvs _ tyvars) typ _) = do
    addDeltaAnnotation GHC.AnnType
    annotatePC ln
    mapM_ annotatePC tyvars
    addDeltaAnnotation GHC.AnnEqual
    annotatePC typ

  annotateP l (GHC.DataDecl ln (GHC.HsQTvs ns tyVars)
                (GHC.HsDataDefn _ ctx mctyp mk cons mderivs) _) = do
    addDeltaAnnotation GHC.AnnData
    addDeltaAnnotation GHC.AnnNewtype
    annotateMaybe mctyp
    annotatePC ctx
    addDeltaAnnotation GHC.AnnDarrow
    annotateTyClass ln tyVars
    addDeltaAnnotation GHC.AnnDcolon
    annotateMaybe mk
    addDeltaAnnotation GHC.AnnEqual
    addDeltaAnnotation GHC.AnnWhere
    mapM_ annotatePC cons
    annotateMaybe mderivs

  -- -----------------------------------

  annotateP l (GHC.ClassDecl ctx ln (GHC.HsQTvs ns tyVars) fds
                          sigs meths ats atdefs docs _) = do
    addDeltaAnnotation GHC.AnnClass
    annotatePC ctx

    annotateTyClass ln tyVars

    addDeltaAnnotation GHC.AnnVbar
    mapM_ annotatePC fds
    addDeltaAnnotation GHC.AnnWhere
    addDeltaAnnotation GHC.AnnOpenC -- '{'
    addDeltaAnnotationsInside GHC.AnnSemi
    applyListAnnotations (prepareListAnnotation sigs
                       ++ prepareListAnnotation (GHC.bagToList meths)
                       ++ prepareListAnnotation ats
                       ++ prepareListAnnotation atdefs
                       ++ prepareListAnnotation docs
                         )
    addDeltaAnnotation GHC.AnnCloseC -- '}'

-- ---------------------------------------------------------------------

annotateTyClass ln tyVars = do
    addDeltaAnnotations GHC.AnnOpenP
    applyListAnnotations (prepareListAnnotation [ln]
                      ++ prepareListAnnotation (take 2 tyVars))
    addDeltaAnnotations GHC.AnnCloseP
    mapM_ annotatePC (drop 2 tyVars)

-- ---------------------------------------------------------------------

instance (GenericCon name,AnnotateP name, GHC.OutputableBndr name)
   => AnnotateP (GHC.FamilyDecl name) where
  annotateP l (GHC.FamilyDecl info ln (GHC.HsQTvs _ tyvars) mkind) = do
    addDeltaAnnotation GHC.AnnType
    addDeltaAnnotation GHC.AnnData
    addDeltaAnnotation GHC.AnnFamily
    annotatePC ln
    mapM_ annotatePC tyvars
    addDeltaAnnotation GHC.AnnDcolon
    annotateMaybe mkind
    addDeltaAnnotation GHC.AnnWhere
    addDeltaAnnotation GHC.AnnOpenC -- {
    case info of
      GHC.ClosedTypeFamily eqns -> mapM_ annotatePC eqns
      _ -> return ()
    case info of
      GHC.ClosedTypeFamily eqns -> mapM_ annotatePC eqns
      _ -> return ()
    addDeltaAnnotation GHC.AnnCloseC -- }

-- ---------------------------------------------------------------------

instance (GenericCon name,AnnotateP name,GHC.OutputableBndr name)
   => AnnotateP (GHC.TyFamInstEqn name) where
  annotateP l (GHC.TyFamEqn ln (GHC.HsWB pats _ _ _) typ) = do
    annotatePC ln
    mapM_ annotatePC pats
    addDeltaAnnotation GHC.AnnEqual
    annotatePC typ


-- ---------------------------------------------------------------------

instance (GenericCon name,AnnotateP name,GHC.OutputableBndr name)
  => AnnotateP (GHC.TyFamDefltEqn name) where
  annotateP l (GHC.TyFamEqn ln (GHC.HsQTvs ns bndrs) typ) = do
    annotatePC ln
    mapM_ annotatePC bndrs
    addDeltaAnnotation GHC.AnnEqual
    annotatePC typ

-- ---------------------------------------------------------------------

-- TODO: modify lexer etc, in the meantime to not set haddock flag
instance AnnotateP GHC.DocDecl where
  annotateP l _ = addDeltaAnnotationExt l GHC.AnnVal

-- ---------------------------------------------------------------------

annotateDataDefn :: (GenericCon name,GHC.OutputableBndr name,AnnotateP name) => GHC.SrcSpan -> GHC.HsDataDefn name -> AP ()
annotateDataDefn l (GHC.HsDataDefn _ ctx typ mk cons mderivs) = do
  annotatePC ctx
  annotateMaybe typ
  annotateMaybe mk
  mapM_ annotatePC cons
  case mderivs of
    Nothing -> return ()
    Just d -> annotatePC d

-- ---------------------------------------------------------------------

-- Note: GHC.HsContext name aliases to here too
instance (GenericCon name,GHC.OutputableBndr name,AnnotateP name)
     => AnnotateP [GHC.LHsType name] where
  annotateP l ts = do
    return () `debug` ("annotateP.HsContext:l=" ++ showGhc l)
    addDeltaAnnotation GHC.AnnDeriving
    addDeltaAnnotation GHC.AnnOpenP
    mapM_ annotatePC ts
    -- addDeltaAnnotation GHC.AnnUnit -- for empty context
    addDeltaAnnotation GHC.AnnCloseP
    addDeltaAnnotation GHC.AnnDarrow

-- ---------------------------------------------------------------------

instance (GenericCon name,AnnotateP name,GHC.OutputableBndr name)
      => AnnotateP (GHC.ConDecl name) where
  annotateP l (GHC.ConDecl lns exp (GHC.HsQTvs _ns bndrs) ctx
                         dets res _ _) = do
    case res of
      GHC.ResTyH98 -> do
        addDeltaAnnotation GHC.AnnForall
        mapM_ annotatePC bndrs
        addDeltaAnnotation GHC.AnnDot

        annotatePC ctx
        addDeltaAnnotation GHC.AnnDarrow

        case dets of
          GHC.InfixCon _ _ -> return ()
          _ -> mapM_ annotatePC lns

        annotateHsConDeclDetails lns dets

      GHC.ResTyGADT ls ty -> do
        -- only print names if not infix
        case dets of
          GHC.InfixCon _ _ -> return ()
          _ -> mapM_ annotatePC lns

        annotateHsConDeclDetails lns dets

        addDeltaAnnotation GHC.AnnDcolon

        annotatePC (GHC.L ls (ResTyGADTHook bndrs))
        {-
        addDeltaAnnotation GHC.AnnForall
        mapM_ annotatePC bndrs
        addDeltaAnnotation GHC.AnnDot
        -}

        annotatePC ctx
        addDeltaAnnotation GHC.AnnDarrow

        annotatePC ty

        -- annotateHsConDeclDetails lns dets

    addDeltaAnnotation GHC.AnnVbar

-- ---------------------------------------------------------------------

instance (GenericCon name,GHC.OutputableBndr name,AnnotateP name) =>
              AnnotateP (ResTyGADTHook name) where
  annotateP _ (ResTyGADTHook bndrs) = do
    addDeltaAnnotation GHC.AnnForall
    mapM_ annotatePC bndrs
    addDeltaAnnotation GHC.AnnDot

-- ---------------------------------------------------------------------

instance (AnnotateP name,AnnotateP a) =>
              AnnotateP (GHC.HsRecField name (GHC.Located a)) where
  annotateP l (GHC.HsRecField n e _) = do
    annotatePC n
    addDeltaAnnotation GHC.AnnEqual
    annotatePC e

-- ---------------------------------------------------------------------

instance (AnnotateP name)
    => AnnotateP (GHC.FunDep (GHC.Located name)) where

  annotateP l (ls,rs) = do
    mapM_ annotatePC ls
    addDeltaAnnotation GHC.AnnRarrow
    mapM_ annotatePC rs

-- ---------------------------------------------------------------------

instance AnnotateP (GHC.CType) where
  annotateP l _ = do
    addDeltaAnnotation GHC.AnnOpen
    addDeltaAnnotation GHC.AnnHeader
    addDeltaAnnotation GHC.AnnVal
    addDeltaAnnotation GHC.AnnClose

-- ---------------------------------------------------------------------

getHsLocalBindsSrcSpan :: (GHC.HsLocalBinds name) -> Maybe GHC.SrcSpan
getHsLocalBindsSrcSpan(GHC.HsValBinds (GHC.ValBindsIn binds sigs)) = r
  where
    r = end (GHC.bagToList binds) sigs

    end [] [] = Nothing
    end [] ys = Just $ GHC.getLoc (last ys)
    end xs [] = Just $ GHC.getLoc (last xs)
    end xs ys = if GHC.getLoc (last xs) < GHC.getLoc (last ys)
                  then Just $ GHC.getLoc (last ys)
                  else Just $ GHC.getLoc (last xs)

getHsLocalBindsSrcSpan (GHC.HsValBinds _) = Nothing
getHsLocalBindsSrcSpan (GHC.HsIPBinds vb) = Nothing
getHsLocalBindsSrcSpan (GHC.EmptyLocalBinds) = Nothing

-- ---------------------------------------------------------------------

getListSpan :: [GHC.Located e] -> [Span]
getListSpan xs = map ss2span $ getListSrcSpan xs

getListSrcSpan :: [GHC.Located e] -> [GHC.SrcSpan]
getListSrcSpan [] = []
getListSrcSpan xs = [GHC.mkSrcSpan (GHC.srcSpanStart (GHC.getLoc (ghead "getListSrcSpan" xs)))
                                   (GHC.srcSpanEnd   (GHC.getLoc (glast "getListSrcSpan" xs)))
                    ]

getListSpans :: [GHC.Located e] -> [Span]
getListSpans xs = map (ss2span . GHC.getLoc) xs


commentPos :: Comment -> (Pos,Pos)
commentPos (Comment _ p _) = p

dcommentPos :: DComment -> (DeltaPos,DeltaPos)
dcommentPos (DComment _ p _) = p


-- ---------------------------------------------------------------------

-- | Given an enclosing Span @(p,e)@, and a list of sub SrcSpans @ds@,
-- identify all comments that are in @(p,e)@ but not in @ds@, and convert
-- them to be DComments relative to @p@
localComments :: Span -> [Comment] -> [Span] -> ([DComment],[Comment])
localComments pin cs ds = r
  `debug` ("localComments:(p,ds,r):" ++ show ((p,e),ds,r))
  where
    r = (map (\c -> deltaComment p c) matches,misses ++ missesRest)
    (p,e) = if pin == ((1,1),(1,1))
               then  ((1,1),(99999999,1))
               else pin

    (matches,misses) = partition notSub cs'
    (cs',missesRest) = partition (\(Comment _ com _) -> isSubPos com (p,e)) cs

    notSub :: Comment -> Bool
    notSub (Comment _ com _) = not $ any (\sub -> isSubPos com sub) ds

    isSubPos (subs,sube) (parents,parente)
      = parents <= subs && parente >= sube

-- ---------------------------------------------------------------------

undeltaComment :: Pos -> DComment -> Comment
undeltaComment l (DComment b (dps,dpe) s) = Comment b ((undelta l dps),(undelta l dpe)) s

deltaComment :: Pos -> Comment -> DComment
deltaComment l (Comment b (s,e) str)
  = DComment b ((ss2deltaP l s),(ss2deltaP l e)) str

-- ---------------------------------------------------------------------

deriving instance Eq GHC.Token

ghcIsComment :: PosToken -> Bool
ghcIsComment ((GHC.L _ (GHC.ITdocCommentNext _)),_s)  = True
ghcIsComment ((GHC.L _ (GHC.ITdocCommentPrev _)),_s)  = True
ghcIsComment ((GHC.L _ (GHC.ITdocCommentNamed _)),_s) = True
ghcIsComment ((GHC.L _ (GHC.ITdocSection _ _)),_s)    = True
ghcIsComment ((GHC.L _ (GHC.ITdocOptions _)),_s)      = True
ghcIsComment ((GHC.L _ (GHC.ITdocOptionsOld _)),_s)   = True
ghcIsComment ((GHC.L _ (GHC.ITlineComment _)),_s)     = True
ghcIsComment ((GHC.L _ (GHC.ITblockComment _)),_s)    = True
ghcIsComment ((GHC.L _ _),_s)                         = False

ghcIsMultiLine :: GHC.Located GHC.AnnotationComment -> Bool
ghcIsMultiLine (GHC.L _ (GHC.AnnDocCommentNext _))  = False
ghcIsMultiLine (GHC.L _ (GHC.AnnDocCommentPrev _))  = False
ghcIsMultiLine (GHC.L _ (GHC.AnnDocCommentNamed _)) = False
ghcIsMultiLine (GHC.L _ (GHC.AnnDocSection _ _))    = False
ghcIsMultiLine (GHC.L _ (GHC.AnnDocOptions _))      = False
ghcIsMultiLine (GHC.L _ (GHC.AnnDocOptionsOld _))   = False
ghcIsMultiLine (GHC.L _ (GHC.AnnLineComment _))     = False
ghcIsMultiLine (GHC.L _ (GHC.AnnBlockComment _))    = True

ghcCommentText :: GHC.Located GHC.AnnotationComment -> String
ghcCommentText (GHC.L _ (GHC.AnnDocCommentNext s))  = s
ghcCommentText (GHC.L _ (GHC.AnnDocCommentPrev s))  = s
ghcCommentText (GHC.L _ (GHC.AnnDocCommentNamed s)) = s
ghcCommentText (GHC.L _ (GHC.AnnDocSection _ s))    = s
ghcCommentText (GHC.L _ (GHC.AnnDocOptions s))      = s
ghcCommentText (GHC.L _ (GHC.AnnDocOptionsOld s))   = s
ghcCommentText (GHC.L _ (GHC.AnnLineComment s))     = s
ghcCommentText (GHC.L _ (GHC.AnnBlockComment s))    = "{-" ++ s ++ "-}"

ghcIsBlank :: PosToken -> Bool
ghcIsBlank (_,s)  = s == ""

ghcIsBlankOrComment :: PosToken -> Bool
ghcIsBlankOrComment t = ghcIsBlank t || ghcIsComment t

-- ---------------------------------------------------------------------

-- | The '[SrcSpan]' should have zero or one element
maybeSrcSpan :: Maybe (GHC.Located a) -> [GHC.SrcSpan]
maybeSrcSpan (Just (GHC.L ss _)) = [ss]
maybeSrcSpan _ = []

-- | The '[SrcSpan]' should have zero or one element
deltaFromMaybeSrcSpans :: [GHC.SrcSpan] -> [GHC.SrcSpan] -> Maybe DeltaPos
deltaFromMaybeSrcSpans [ss1] [ss2] = Just (deltaFromSrcSpans ss1 ss2)
deltaFromMaybeSrcSpans _ _ = Nothing

deltaFromLastSrcSpan :: [GHC.SrcSpan] -> [GHC.SrcSpan] -> Maybe DeltaPos
deltaFromLastSrcSpan [] _ = error $ "deltaFromLastSrcSpan: no last SrcSpan"
deltaFromLastSrcSpan sss ms = deltaFromMaybeSrcSpans [last sss] ms

-- | Create a delta covering the gap between the end of the first
-- @SrcSpan@ and the start of the second.
deltaFromSrcSpans :: GHC.SrcSpan -> GHC.SrcSpan -> DeltaPos
deltaFromSrcSpans ss1 ss2 = ss2delta (ss2posEnd ss1) ss2

ss2delta :: Pos -> GHC.SrcSpan -> DeltaPos
ss2delta ref ss = ss2deltaP ref (ss2pos ss)

-- | Convert the start of the second @Pos@ to be an offset from the
-- first. The assumption is the reference starts before the second @Pos@
ss2deltaP :: Pos -> Pos -> DeltaPos
ss2deltaP (refl,refc) (l,c) = DP (lo,co)
  where
    lo = l - refl
    co = if lo == 0 then c - refc
                    else c

undelta :: Pos -> DeltaPos -> Pos
undelta (l,c) (DP (dl,dc)) = (fl,fc)
  where
    fl = l + dl
    fc = if dl == 0 then c + dc else dc

-- prop_delta :: TODO

ss2pos :: GHC.SrcSpan -> Pos
ss2pos ss = (srcSpanStartLine ss,srcSpanStartColumn ss)

ss2posEnd :: GHC.SrcSpan -> Pos
ss2posEnd ss = (srcSpanEndLine ss,srcSpanEndColumn ss)

ss2span :: GHC.SrcSpan -> Span
ss2span ss = (ss2pos ss,ss2posEnd ss)

srcSpanStart :: GHC.SrcSpan -> Pos
srcSpanStart ss = (srcSpanStartLine ss,srcSpanStartColumn ss)

srcSpanEnd :: GHC.SrcSpan -> Pos
srcSpanEnd ss = (srcSpanEndLine ss,srcSpanEndColumn ss)


srcSpanEndColumn :: GHC.SrcSpan -> Int
srcSpanEndColumn (GHC.RealSrcSpan s) = GHC.srcSpanEndCol s
srcSpanEndColumn _ = 0

srcSpanStartColumn :: GHC.SrcSpan -> Int
srcSpanStartColumn (GHC.RealSrcSpan s) = GHC.srcSpanStartCol s
srcSpanStartColumn _ = 0

srcSpanEndLine :: GHC.SrcSpan -> Int
srcSpanEndLine (GHC.RealSrcSpan s) = GHC.srcSpanEndLine s
srcSpanEndLine _ = 0

srcSpanStartLine :: GHC.SrcSpan -> Int
srcSpanStartLine (GHC.RealSrcSpan s) = GHC.srcSpanStartLine s
srcSpanStartLine _ = 0

nullSpan :: Span
nullSpan = ((0,0),(0,0))

-- ---------------------------------------------------------------------

isPointSrcSpan :: GHC.SrcSpan -> Bool
isPointSrcSpan ss = s == e where (s,e) = ss2span ss

-- ---------------------------------------------------------------------

tokenSpan :: PosToken -> GHC.SrcSpan
tokenSpan ((GHC.L l _),_s) = l

tokenPos :: PosToken -> Pos
tokenPos ((GHC.L l _),_s) = srcSpanStart l

tokenPosEnd :: PosToken -> Pos
tokenPosEnd ((GHC.L l _),_s) = srcSpanEnd l

tokenString :: PosToken -> String
tokenString (_,s) = s

-- ---------------------------------------------------------------------

splitToks:: (GHC.SrcSpan,GHC.SrcSpan) -> [PosToken]->([PosToken],[PosToken],[PosToken])
splitToks (startPos, endPos) toks =
  let (toks1,toks2)   = break (\t -> tokenSpan t >= startPos) toks
      (toks21,toks22) = break (\t -> tokenSpan t >=   endPos) toks2
  in
    (toks1,toks21,toks22)

-- ---------------------------------------------------------------------

splitToksForSpan:: GHC.SrcSpan -> [PosToken] -> ([PosToken],[PosToken],[PosToken])
splitToksForSpan ss toks =
  let (toks1,toks2)   = break (\t -> tokenPos t >= srcSpanStart ss) toks
      (toks21,toks22) = break (\t -> tokenPos t >= srcSpanEnd   ss) toks2
  in
    (toks1,toks21,toks22)

-- ---------------------------------------------------------------------

isListComp :: GHC.HsStmtContext name -> Bool
isListComp cts = case cts of
          GHC.ListComp  -> True
          GHC.MonadComp -> True
          GHC.PArrComp  -> True

          GHC.DoExpr       -> False
          GHC.MDoExpr      -> False
          GHC.ArrowExpr    -> False
          GHC.GhciStmtCtxt -> False

          GHC.PatGuard {}      -> False
          GHC.ParStmtCtxt {}   -> False
          GHC.TransStmtCtxt {} -> False

-- ---------------------------------------------------------------------

isSymbolRdrName :: GHC.RdrName -> Bool
isSymbolRdrName n = GHC.isSymOcc $ GHC.rdrNameOcc n

rdrName2String :: GHC.RdrName -> String
rdrName2String r =
  case GHC.isExact_maybe r of
    Just n  -> name2String n
    Nothing ->
      case r of
        GHC.Unqual occ -> GHC.occNameString $ GHC.rdrNameOcc r
        GHC.Qual modname occ -> GHC.moduleNameString modname ++ "."
                            ++ (GHC.occNameString $ GHC.rdrNameOcc r)

name2String :: GHC.Name -> String
name2String name = showGhc name

-- |Show a GHC API structure
showGhc :: (GHC.Outputable a) => a -> String
#if __GLASGOW_HASKELL__ > 706
showGhc x = GHC.showPpr GHC.unsafeGlobalDynFlags x
#elif __GLASGOW_HASKELL__ > 704
showGhc x = GHC.showSDoc GHC.tracingDynFlags $ GHC.ppr x
#else
showGhc x = GHC.showSDoc                     $ GHC.ppr x
#endif


-- |Show a GHC API structure
showGhcDebug :: (GHC.Outputable a) => a -> String
#if __GLASGOW_HASKELL__ > 706
showGhcDebug x = GHC.showSDocDebug GHC.unsafeGlobalDynFlags (GHC.ppr x)
#else
#if __GLASGOW_HASKELL__ > 704
showGhcDebug x = GHC.showSDoc GHC.tracingDynFlags $ GHC.ppr x
#else
showGhcDebug x = GHC.showSDoc                     $ GHC.ppr x
#endif
#endif

-- ---------------------------------------------------------------------

instance Show (GHC.GenLocated GHC.SrcSpan GHC.Token) where
  show t@(GHC.L l tok) = show ((srcSpanStart l, srcSpanEnd l),tok)

-- ---------------------------------------------------------------------

pp a = GHC.showPpr GHC.unsafeGlobalDynFlags a

-- ---------------------------------------------------------------------
-- Putting these here for the time being, to avoid import loops

ghead :: String -> [a] -> a
ghead  info []    = error $ "ghead "++info++" []"
ghead _info (h:_) = h

glast :: String -> [a] -> a
glast  info []    = error $ "glast " ++ info ++ " []"
glast _info h     = last h

gtail :: String -> [a] -> [a]
gtail  info []   = error $ "gtail " ++ info ++ " []"
gtail _info h    = tail h

gfromJust :: [Char] -> Maybe a -> a
gfromJust _info (Just h) = h
gfromJust  info Nothing = error $ "gfromJust " ++ info ++ " Nothing"

-- -------------------------------------------------------------------..
-- Copied from MissingH, does not compile with HEAD


{- | Merge two sorted lists into a single, sorted whole.

Example:

> merge [1,3,5] [1,2,4,6] -> [1,1,2,3,4,5,6]

QuickCheck test property:

prop_merge xs ys =
    merge (sort xs) (sort ys) == sort (xs ++ ys)
          where types = xs :: [Int]
-}
merge ::  (Ord a) => [a] -> [a] -> [a]
merge = mergeBy (compare)

{- | Merge two sorted lists using into a single, sorted whole,
allowing the programmer to specify the comparison function.

QuickCheck test property:

prop_mergeBy xs ys =
    mergeBy cmp (sortBy cmp xs) (sortBy cmp ys) == sortBy cmp (xs ++ ys)
          where types = xs :: [ (Int, Int) ]
                cmp (x1,_) (x2,_) = compare x1 x2
-}
mergeBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
mergeBy cmp [] ys = ys
mergeBy cmp xs [] = xs
mergeBy cmp (allx@(x:xs)) (ally@(y:ys))
        -- Ordering derives Eq, Ord, so the comparison below is valid.
        -- Explanation left as an exercise for the reader.
        -- Someone please put this code out of its misery.
    | (x `cmp` y) <= EQ = x : mergeBy cmp xs ally
    | otherwise = y : mergeBy cmp allx ys


maybeL :: Show a => a -> [a] -> a
maybeL def [] = def
maybeL _ [x] = x
maybeL def xs = error $ "maybeL " ++ show (def,xs)
