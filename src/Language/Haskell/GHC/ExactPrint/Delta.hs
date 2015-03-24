{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
module Language.Haskell.GHC.ExactPrint.Delta  (relativiseApiAnns) where

import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.RWS
import Control.Applicative
import Control.Monad.Trans.Free
import Data.Data
import Data.List
import Data.Maybe

import Language.Haskell.GHC.ExactPrint.Types
import Language.Haskell.GHC.ExactPrint.Utils
import Language.Haskell.GHC.ExactPrint.Annotate (AnnotationF(..), Annotated
                                                , markLocated, Annotate(..))

import qualified GHC            as GHC
import qualified SrcLoc         as GHC


import qualified Data.Map as Map


-- ---------------------------------------------------------------------
--
-- | Type used in the Delta Monad. The state variables maintain
--    - the current SrcSpan and the constructor of the thing it encloses
--      as a stack to the root of the AST as it is traversed,
--    - the srcspan of the last thing annotated, to calculate delta's from
--    - extra data needing to be stored in the monad
--    - the annotations provided by GHC
type Delta a = RWS DeltaStack DeltaWriter DeltaState a

-- | Transform concrete annotations into relative annotations which are
-- more useful when transforming an AST.
relativiseApiAnns :: Annotate ast
                  => GHC.Located ast
                  -> GHC.ApiAnns
                  -> Anns
relativiseApiAnns modu@(GHC.L ss _) ghcAnns
   = runDelta (markLocated modu) ghcAnns ss


runDelta :: Annotated () -> GHC.ApiAnns -> GHC.SrcSpan -> Anns
runDelta action ga priorEnd =
  ($ mempty) . appEndo . finalAnns . snd
  . (\next -> execRWS next initialDeltaStack (defaultDeltaState priorEnd ga))
  . simpleInterpret $ action

-- ---------------------------------------------------------------------

data DeltaState = DeltaState
             { -- | Position reached when processing the last element
               priorEndPosition :: GHC.SrcSpan
               -- | Ordered list of comments still to be allocated
             , apComments :: [Comment]
               -- | The original GHC Delta Annotations
             , apAnns :: GHC.ApiAnns
             }

data DeltaStack = DeltaStack
               { -- | Current `SrcSpan`
                 curSrcSpan :: GHC.SrcSpan
                 -- | Constuctor of current AST element, useful for
                 -- debugging
               , annConName :: AnnConName
                 -- | Start column of the current layout block
               , layoutStart :: Col
               }

initialDeltaStack :: DeltaStack
initialDeltaStack =
  DeltaStack
    { curSrcSpan = GHC.noSrcSpan
    , annConName = annGetConstr ()
    , layoutStart = 0
    }

defaultDeltaState :: GHC.SrcSpan -> GHC.ApiAnns -> DeltaState
defaultDeltaState priorEnd ga =
    DeltaState
      { priorEndPosition = priorEnd
      , apComments = cs
      , apAnns     = ga    -- $
      }
  where
    cs :: [Comment]
    cs = flattenedComments ga

    flattenedComments :: GHC.ApiAnns -> [Comment]
    flattenedComments (_,cm) =
      map tokComment . GHC.sortLocated . concat $ Map.elems cm

    tokComment :: GHC.Located GHC.AnnotationComment -> Comment
    tokComment t@(GHC.L lt _) = Comment (ss2span lt) (ghcCommentText t)


data DeltaWriter = DeltaWriter
  { -- Final list of annotations
    finalAnns :: Endo (Map.Map AnnKey Annotation)
    -- Used locally to pass Keywords, delta pairs relevant to a specific
    -- subtree to the parent.
  , annKds :: [(KeywordId, DeltaPos)]
    -- Used locally to report a subtrees aderhence to haskell's layout
    -- rules.
  , propOffset :: First Int -- Used to pass the offset upwards
  }

-- Writer helpers

tellFinalAnn :: (AnnKey, Annotation) -> Delta ()
tellFinalAnn (k, v) =
  tell (mempty { finalAnns = Endo (Map.insertWith (<>) k v) })

tellKd :: (KeywordId, DeltaPos) -> Delta ()
tellKd kd = tell (mempty { annKds = [kd] })


instance Monoid DeltaWriter where
  mempty = DeltaWriter mempty mempty mempty
  (DeltaWriter a b e) `mappend` (DeltaWriter c d f) = DeltaWriter (a <> c) (b <> d) (e <> f)

-----------------------------------
-- Interpretation code

simpleInterpret :: Annotated a -> Delta a
simpleInterpret = iterTM go
  where
    go :: AnnotationF (Delta a) -> Delta a
    go (MarkEOF next) = addEofAnnotation >> next
    go (MarkPrim kwid _ next) =
      addDeltaAnnotation kwid >> next
    go (MarkOutside akwid kwid next) =
      addDeltaAnnotationsOutside akwid kwid >> next
    go (MarkInside akwid next) =
      addDeltaAnnotationsInside akwid >> next
    go (MarkMany akwid next) = addDeltaAnnotations akwid >> next
    go (MarkOffsetPrim akwid n _ next) = addDeltaAnnotationLs akwid n >> next
    go (MarkAfter akwid next) = addDeltaAnnotationAfter akwid >> next
    go (WithAST lss layoutflag prog next) =
      withAST lss layoutflag (simpleInterpret prog) >> next
    go (OutputKD (kwid, (_, dp)) next) = tellKd (dp, kwid) >> next
    go (CountAnns kwid next) = countAnnsDelta kwid >>= next
    go (SetLayoutFlag kwid action next) = setLayoutFlag kwid (simpleInterpret action)  >> next
    go (MarkExternal ss akwid _ next) = addDeltaAnnotationExt ss akwid >> next


-- | Used specifically for "HsLet"
setLayoutFlag :: GHC.AnnKeywordId -> Delta () -> Delta ()
setLayoutFlag kwid action = do
  c <-  srcSpanStartColumn . head <$> getAnnotationDelta kwid
  tell (mempty { propOffset = First (Just c) })
  local (\s -> s { layoutStart = c }) action


-- -------------------------------------

getSrcSpan :: Delta GHC.SrcSpan
getSrcSpan = asks curSrcSpan

withSrcSpanDelta :: Data a => (GHC.Located a) -> Delta b -> Delta b
withSrcSpanDelta (GHC.L l a) =
  local (\s -> s { curSrcSpan = l
                 , annConName = annGetConstr a
                 })



getUnallocatedComments :: Delta [Comment]
getUnallocatedComments = gets apComments

putUnallocatedComments :: [Comment] -> Delta ()
putUnallocatedComments cs = modify (\s -> s { apComments = cs } )

-- ---------------------------------------------------------------------

adjustDeltaForOffsetM :: DeltaPos -> Delta DeltaPos
adjustDeltaForOffsetM dp = do
  colOffset <- asks layoutStart
  return (adjustDeltaForOffset colOffset dp)

adjustDeltaForOffset :: Int -> DeltaPos -> DeltaPos
adjustDeltaForOffset _colOffset dp@(DP (0,_)) = dp -- same line
adjustDeltaForOffset  colOffset    (DP (l,c)) = DP (l,c - colOffset)

-- ---------------------------------------------------------------------

getPriorEnd :: Delta GHC.SrcSpan
getPriorEnd = gets priorEndPosition

setPriorEnd :: GHC.SrcSpan -> Delta ()
setPriorEnd pe = modify (\s -> s { priorEndPosition = pe })

setLayoutOffset :: Int -> Delta a -> Delta a
setLayoutOffset lhs = local (\s -> s { layoutStart = lhs })

-- -------------------------------------

getAnnotationDelta :: GHC.AnnKeywordId -> Delta [GHC.SrcSpan]
getAnnotationDelta an = do
    ga <- gets apAnns
    ss <- getSrcSpan
    return $ GHC.getAnnotation ga ss an

getAndRemoveAnnotationDelta :: GHC.SrcSpan -> GHC.AnnKeywordId -> Delta [GHC.SrcSpan]
getAndRemoveAnnotationDelta sp an = do
    ga <- gets apAnns
    let (r,ga') = GHC.getAndRemoveAnnotation ga sp an
    r <$ modify (\s -> s { apAnns = ga' })

-- -------------------------------------

-- |Add some annotation to the currently active SrcSpan
addAnnotationsDelta :: Annotation -> Delta ()
addAnnotationsDelta ann = do
    l <- ask
    tellFinalAnn ((getAnnKey l),ann)

getAnnKey :: DeltaStack -> AnnKey
getAnnKey DeltaStack {curSrcSpan, annConName} = AnnKey curSrcSpan annConName

-- -------------------------------------

addAnnDeltaPos :: KeywordId -> DeltaPos -> Delta ()
addAnnDeltaPos kw dp = tellKd (kw, dp)

-- -------------------------------------


-- | Enter a new AST element. Maintain SrcSpan stack
withAST :: Data a => GHC.Located a -> LayoutFlag -> Delta b -> Delta b
withAST lss@(GHC.L ss _) layout action = do
  -- Calculate offset required to get to the start of the SrcSPan
  pe <- getPriorEnd
  off <- asks layoutStart
  let whenLayout = case layout of
                        LayoutRules -> setLayoutOffset (srcSpanStartColumn ss)
                        NoLayoutRules -> id
  (whenLayout .  withSrcSpanDelta lss) (do

    -- maskWriter only allows the finalAnns through
    let maskWriter s = s { annKds = []
                         , propOffset = First Nothing }

    (res, w) <-
      (censor maskWriter (listen action))
    let edp = adjustDeltaForOffset
                -- Use the propagated offset if one is set
                (fromMaybe off (getFirst $ propOffset w))
                  (deltaFromSrcSpans pe ss)
    let kds = annKds w
    addAnnotationsDelta Ann
                          { annEntryDelta = edp
                          , annDelta   = (srcSpanStartColumn ss - off)
                          , annsDP     = kds }
    --  `debug` ("leaveAST:(ss,finaledp,dp,nl,kds)=" ++ show (showGhc ss,edp,dp,nl,kds))
    return res)

-- ---------------------------------------------------------------------

-- |Split the ordered list of comments into ones that occur prior to
-- the given SrcSpan and the rest
allocatePriorComments :: [Comment] -> GHC.SrcSpan -> ([Comment],[Comment])
allocatePriorComments cs ss = partition isPrior cs
  where
    (start,_) = ss2span ss
    isPrior (Comment s _)  = (fst s) < start
      `debug` ("allocatePriorComments:(s,ss,cond)=" ++ showGhc (s,ss,(fst s) < start))

-- ---------------------------------------------------------------------

addAnnotationWorker :: KeywordId -> GHC.SrcSpan -> Delta ()
addAnnotationWorker ann pa = do
  if not (isPointSrcSpan pa)
    then do
      pe <- getPriorEnd
      ss <- getSrcSpan
      let p = deltaFromSrcSpans pe pa
      case (ann,isGoodDelta p) of
        (G GHC.AnnComma,False) -> return ()
        (G GHC.AnnSemi, False) -> return ()
        (G GHC.AnnOpen, False) -> return ()
        (G GHC.AnnClose,False) -> return ()
        _ -> do
          cs <- getUnallocatedComments
          let (allocated,cs') = allocatePriorComments cs pa
          putUnallocatedComments cs'
          return () `debug`("addAnnotationWorker:(ss,pa,allocated,cs)=" ++ showGhc (ss,pa,allocated,cs))
          mapM_ addDeltaComment allocated
          p' <- adjustDeltaForOffsetM p
          addAnnDeltaPos ann p'
          setPriorEnd pa
              -- `debug` ("addDeltaAnnotationWorker:(ss,pe,pa,p,ann)=" ++ show (ss2span ss,ss2span pe,ss2span pa,p,ann))
    else do
      return ()
          -- `debug` ("addDeltaAnnotationWorker::point span:(ss,ma,ann)=" ++ show (ss2span pa,ann))

-- ---------------------------------------------------------------------

addDeltaComment :: Comment -> Delta ()
addDeltaComment (Comment paspan str) = do
  let pa = span2ss paspan
  pe <- getPriorEnd
  let p = deltaFromSrcSpans pe pa
  p' <- adjustDeltaForOffsetM p
  setPriorEnd pa
  let e = ss2deltaP (ss2posEnd pe) (snd paspan)
  e' <- adjustDeltaForOffsetM e
  addAnnDeltaPos (AnnComment (DComment (p',e') str)) p'

-- ---------------------------------------------------------------------

-- | Look up and add a Delta annotation at the current position, and
-- advance the position to the end of the annotation
addDeltaAnnotation :: GHC.AnnKeywordId -> Delta ()
addDeltaAnnotation ann = do
  ss <- getSrcSpan
  when (ann == GHC.AnnVal) (debugM (showGhc ss))
  ma <- getAnnotationDelta ann
  when (ann == GHC.AnnVal && null ma) (debugM "empty")
  case nub ma of -- ++AZ++ TODO: get rid of duplicates earlier
    [] -> return () `debug` ("addDeltaAnnotation empty ma for:" ++ show ann)
    [pa] -> addAnnotationWorker (G ann) pa
    _ -> error $ "addDeltaAnnotation:(ss,ann,ma)=" ++ showGhc (ss,ann,ma)

-- | Look up and add a Delta annotation appearing beyond the current
-- SrcSpan at the current position, and advance the position to the
-- end of the annotation
addDeltaAnnotationAfter :: GHC.AnnKeywordId -> Delta ()
addDeltaAnnotationAfter ann = do
  ss <- getSrcSpan
  ma <- getAnnotationDelta ann
  let ma' = filter (\s -> not (GHC.isSubspanOf s ss)) ma
  case ma' of
    [] -> return () `debug` ("addDeltaAnnotation empty ma")
    [pa] -> addAnnotationWorker (G ann) pa
    _ -> error $ "addDeltaAnnotation:(ss,ann,ma)=" ++ showGhc (ss,ann,ma)

-- | Look up and add a Delta annotation at the current position, and
-- advance the position to the end of the annotation
addDeltaAnnotationLs :: GHC.AnnKeywordId -> Int -> Delta ()
addDeltaAnnotationLs ann off = do
  ma <- getAnnotationDelta ann
  case (drop off ma) of
    [] -> return ()
        -- `debug` ("addDeltaAnnotationLs:missed:(off,pe,ann,ma)=" ++ show (off,ss2span pe,ann,fmap ss2span ma))
    (pa:_) -> addAnnotationWorker (G ann) pa

-- | Look up and add possibly multiple Delta annotation at the current
-- position, and advance the position to the end of the annotations
addDeltaAnnotations :: GHC.AnnKeywordId -> Delta ()
addDeltaAnnotations ann = do
  ma <- getAnnotationDelta ann
  let do_one ap' = addAnnotationWorker (G ann) ap'
                    -- `debug` ("addDeltaAnnotations:do_one:(ap',ann)=" ++ showGhc (ap',ann))
  mapM_ do_one (sort ma)

-- | Look up and add possibly multiple Delta annotations enclosed by
-- the current SrcSpan at the current position, and advance the
-- position to the end of the annotations
addDeltaAnnotationsInside :: GHC.AnnKeywordId -> Delta ()
addDeltaAnnotationsInside ann = do
  ss <- getSrcSpan
  ma <- getAnnotationDelta ann
  let do_one ap' = addAnnotationWorker (G ann) ap'
                    -- `debug` ("addDeltaAnnotations:do_one:(ap',ann)=" ++ showGhc (ap',ann))
  let filtered = (sort $ filter (\s -> GHC.isSubspanOf s ss) ma)
  mapM_ do_one filtered

-- | Look up and add possibly multiple Delta annotations not enclosed by
-- the current SrcSpan at the current position, and advance the
-- position to the end of the annotations
addDeltaAnnotationsOutside :: GHC.AnnKeywordId -> KeywordId -> Delta ()
addDeltaAnnotationsOutside gann ann = do
  ss <- getSrcSpan
  if ss2span ss == ((1,1),(1,1))
    then return ()
    else do
      -- ma <- getAnnotationDelta ss gann
      ma <- getAndRemoveAnnotationDelta ss gann
      let do_one ap' = addAnnotationWorker ann ap'
      mapM_ do_one (sort $ filter (\s -> not (GHC.isSubspanOf s ss)) ma)

-- | Add a Delta annotation at the current position, and advance the
-- position to the end of the annotation
addDeltaAnnotationExt :: GHC.SrcSpan -> GHC.AnnKeywordId -> Delta ()
addDeltaAnnotationExt s ann = do
  addAnnotationWorker (G ann) s

addEofAnnotation :: Delta ()
addEofAnnotation = do
  pe <- getPriorEnd
  ma <- withSrcSpanDelta (GHC.noLoc ()) (getAnnotationDelta GHC.AnnEofPos)
  case ma of
    [] -> return ()
    (pa:pss) -> do
      cs <- getUnallocatedComments
      mapM_ addDeltaComment cs
      let DP (r,c) = deltaFromSrcSpans pe pa
      addAnnDeltaPos (G GHC.AnnEofPos) (DP (r, c - 1))
      setPriorEnd pa `warn` ("Trailing annotations after Eof: " ++ showGhc pss)


countAnnsDelta :: GHC.AnnKeywordId -> Delta Int
countAnnsDelta ann = do
  ma <- getAnnotationDelta ann
  return (length ma)



