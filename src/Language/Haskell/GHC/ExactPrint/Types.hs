{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ConstraintKinds #-}
module Language.Haskell.GHC.ExactPrint.Types
  (
    Comment(..)
  , DComment(..)
  , Pos
  , Span
  , PosToken
  , DeltaPos(..)
  , Annotation(..)
  , annNone
  , Anns,anEP,anF
  , AnnsEP
  , KeywordId(..)
  , GenericCon
  , AnnConName
  , annGetConstr

  , ResTyGADTHook(..)

  , AnnKey
  , AnnKeyF
  , mkAnnKeyEP
  , getAnnotationEP
  , getAndRemoveAnnotationEP

  ) where

import Data.Data
import GHC.Generics
import Generics.Deriving.ConNames

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
import qualified Outputable    as GHC

import qualified Data.Map as Map

-- ---------------------------------------------------------------------

-- | A Haskell comment. The 'Bool' is 'True' if the comment is multi-line, i.e. @{- -}@.
data Comment = Comment Bool Span String
  deriving (Eq,Show,Typeable,Data)

data DComment = DComment Bool (DeltaPos,DeltaPos) String
  deriving (Eq,Show,Typeable,Data)

instance Ord Comment where
  compare (Comment _ p1 _) (Comment _ p2 _) = compare p1 p2

type PosToken = (GHC.Located GHC.Token, String)

type Pos = (Int,Int)
type Span = (Pos,Pos)

newtype DeltaPos = DP (Int,Int) deriving (Show,Eq,Ord,Typeable,Data)

annNone :: Annotation
annNone = Ann [] (DP (0,0))

data Annotation = Ann
  { ann_comments :: ![DComment]
  , ann_delta    :: !DeltaPos -- Do we need this? Yes indeed.
  } deriving (Show,Typeable)


instance Show GHC.RdrName where
  show n = "(a RdrName)"

-- first field carries the comments, second the offsets
type Anns = (AnnsEP,AnnsFinal)
anEP :: Anns -> AnnsEP
anEP (e,_) = e
anF :: Anns -> AnnsFinal
anF  (_,f) = f

-- Holds a GHC.Generics conName of a constructor
data AnnConName = CN String
                 deriving (Eq,Show,Ord)
type GenericCon a = (ConNames (Rep a), Generic a)

annGetConstr :: (GenericCon a)  => a -> AnnConName
annGetConstr a = CN (conNameOf a)


-- | For every @Located a@, use the @SrcSpan@ and GHC.Generics conName of a as the
-- key, to store the standard annotation.
type AnnsEP = Map.Map (GHC.SrcSpan,AnnConName) Annotation

-- | For every SrcSpan, store an annotation as a value where the
-- TypeRep is of the item wrapped in the Value
-- type AnnsUser = Map.Map (GHC.SrcSpan,TypeRep) Value

type AnnsFinal = Map.Map (GHC.SrcSpan,KeywordId) [DeltaPos]

-- We need our own version of keywordid to distinguish between a
-- semi-colon appearing within an AST element and one separating AST
-- elements in a list.
data KeywordId = G GHC.AnnKeywordId
               | AnnSemiSep
               deriving (Eq,Show,Ord)

instance GHC.Outputable KeywordId where
  ppr k     = GHC.text (show k)

instance GHC.Outputable (AnnConName) where
  ppr tr     = GHC.text (show tr)

{-
instance GHC.Outputable TypeRep where
  ppr tr     = GHC.text (show tr)

instance GHC.Outputable ConstrRep where
  ppr tr     = GHC.text (show tr)
-}

instance GHC.Outputable Annotation where
  ppr a     = GHC.text (show a)

instance GHC.Outputable DeltaPos where
  ppr a     = GHC.text (show a)

-- ---------------------------------------------------------------------

-- ResTyGADT has a SrcSpan for the original sigtype, we need to create
-- a type for exactPC and annotatePC
data ResTyGADTHook name = ResTyGADTHook [GHC.LHsTyVarBndr name]
                   deriving (Typeable,Generic)


-- ---------------------------------------------------------------------
{-

Rationale.

We need an offset for every SrcSpan, as well as the comments
associated with it.

We also need optional other annotations for keywords etc.

So perhaps one AnnKey based on the typeRep of a in (Located a), and
another for the user annotation.


-}

type AnnKey  = (GHC.SrcSpan, AnnConName)
type AnnKeyF = (GHC.SrcSpan, KeywordId)

mkAnnKeyEP :: (GenericCon a) => GHC.Located a -> AnnKey
mkAnnKeyEP (GHC.L l a) = (l,annGetConstr a)

deriving instance Ord ConstrRep

getAnnotationEP :: (GenericCon a) => AnnsEP -> GHC.Located a -> Maybe Annotation
getAnnotationEP anns (GHC.L ss a) = Map.lookup (ss, annGetConstr a) anns

getAndRemoveAnnotationEP :: (GenericCon a) => AnnsEP -> GHC.Located a -> (Maybe Annotation,AnnsEP)
getAndRemoveAnnotationEP anns (GHC.L ss a)
 = case Map.lookup (ss, annGetConstr a) anns of
     Nothing  -> (Nothing,anns)
     Just ann -> (Just ann,Map.delete (ss, annGetConstr a) anns)

-- ---------------------------------------------------------------------
-- Generic instances for GHC AST


deriving instance Generic (GHC.HsCmdTop name)
deriving instance Generic (GHC.ForeignDecl name)
deriving instance Generic (GHC.HsRecField name body)
deriving instance Generic (GHC.ConDecl name)
deriving instance Generic (GHC.DocDecl)
deriving instance Generic (GHC.TyFamEqn name pats)
deriving instance Generic (GHC.FamilyDecl name)
deriving instance Generic (GHC.TyClDecl name)
deriving instance Generic (GHC.HsCmd name)
deriving instance Generic (GHC.HsTupArg name)
deriving instance Generic (GHC.HsExpr name)
deriving instance Generic (GHC.StmtLR idL idR body)
deriving instance Generic (GHC.HsWithBndrs name arg)
deriving instance Generic (GHC.HsOverLit name)
deriving instance Generic (GHC.Pat name)
deriving instance Generic (GHC.ConDeclField name)
deriving instance Generic (GHC.HsType name)
deriving instance Generic (GHC.HsTyVarBndr name)
deriving instance Generic (GHC.Sig name)
deriving instance Generic (GHC.GRHS name body)
deriving instance Generic (GHC.Match name body)
deriving instance Generic (GHC.HsIPName)
deriving instance Generic (GHC.IPBind name)
deriving instance Generic (GHC.HsBindLR idL idR)
deriving instance Generic (GHC.HsDocString)
deriving instance Generic (GHC.DataFamInstDecl name)
deriving instance Generic (GHC.TyFamInstDecl name)
deriving instance Generic (GHC.ClsInstDecl name)
deriving instance Generic (GHC.OverlapMode)
deriving instance Generic (GHC.InstDecl name)
deriving instance Generic (GHC.DefaultDecl name)
deriving instance Generic (GHC.DerivDecl name)
deriving instance Generic (GHC.Safety)
deriving instance Generic (GHC.CCallConv)
deriving instance Generic (GHC.CExportSpec)
deriving instance Generic (GHC.FastString)
deriving instance Generic (GHC.WarnDecl name)
deriving instance Generic (GHC.WarnDecls name)
deriving instance Generic (GHC.AnnDecl name)
deriving instance Generic (GHC.RuleBndr name)
deriving instance Generic (GHC.RuleDecl name)
deriving instance Generic (GHC.RuleDecls name)
deriving instance Generic (GHC.VectDecl name)
deriving instance Generic (GHC.SpliceDecl name)
deriving instance Generic (GHC.RoleAnnotDecl name)
deriving instance Generic (GHC.HsQuasiQuote name)
deriving instance Generic (GHC.HsDecl name)
deriving instance Generic (GHC.ImportDecl name)
deriving instance Generic (GHC.IE name)
deriving instance Generic (GHC.WarningTxt)
deriving instance Generic (GHC.HsModule name)

deriving instance Generic (GHC.RdrName)
-- deriving instance Generic (GHC.Name)
deriving instance Generic (GHC.CType)

