{-# LANGUAGE TemplateHaskell, RankNTypes #-}
module Lamdu.Data.Infer.Rule.Types.Rigidity
  ( Level(..)
  , PositionCounts(..), pcOther, pcRigid, pcUnit, pcRecord
  , summaryLevel
  , TypeKnowledge(..)
  , ValKnowledge(..)
  , CountedAs(..)
  , countedAs, counterOf
  , PosType(..), ptCountedAs, ptKnowledge
  , PosVal(..), pvCorrespondingType, pvKnowledge
  , RefRigidityData
  , Rigidity(..), rCounts, rRefs
  , rigidityExprRefs
  ) where

import Control.Applicative ((<$>))
import Control.Lens (Lens')
import Control.Lens.Operators
import Data.Bitraversable (bitraverse)
import Data.Monoid (Last)
import Lamdu.Data.Infer.RefTags (ExprRef, TagExpr)
import qualified Control.Lens as Lens
import qualified Data.OpaqueRef as OR

data Level
  = Other   -- unknown or has discernable information
  | Rigid   -- type or GetVar (even redex getvar, for now)
  | Unit    -- record with only Unit fields (or initial state)
  deriving (Eq, Ord)

data PositionCounts
  = PositionCounts
  { _pcOther :: Int
  , _pcRigid :: Int
  , _pcUnit :: Int
  , _pcRecord :: Int -- kept around for diagnostic purposes
  } deriving (Show)
Lens.makeLenses ''PositionCounts

summaryLevel :: PositionCounts -> Level
summaryLevel counts
  | counts ^. pcOther > 0 = Other
  | counts ^. pcRigid > 0 = Rigid
  | otherwise = Unit

data TypeKnowledge
  = TypeRecord
  | TypeOther
  | TypeUnit
  | TypeType -- Type is type or getvar

data ValKnowledge
  = ValRecord
  | ValOther
  | ValGetParameter

data CountedAs
  = CountedAsRecord
  | CountedAsLevel Level

counterOf :: CountedAs -> Lens' PositionCounts Int
counterOf CountedAsRecord = pcRecord
counterOf (CountedAsLevel Rigid) = pcRigid
counterOf (CountedAsLevel Unit) = pcUnit
counterOf (CountedAsLevel Other) = pcOther

countedAs :: TypeKnowledge -> ValKnowledge -> CountedAs
countedAs TypeRecord _ = CountedAsRecord
countedAs TypeUnit _ = CountedAsLevel Unit
countedAs TypeType _ = CountedAsLevel Rigid
countedAs TypeOther ValRecord = CountedAsRecord
countedAs TypeOther ValGetParameter = CountedAsLevel Rigid
countedAs TypeOther ValOther = CountedAsLevel Other

data PosType = PosType
  { _ptCountedAs :: CountedAs
  , _ptKnowledge :: TypeKnowledge
  }
Lens.makeLenses ''PosType

posTypeExprRefs :: Lens.Traversal' PosType (ExprRef def)
posTypeExprRefs = Lens.ignored

data PosVal def
  = PosVal
  { _pvKnowledge :: ValKnowledge
  , _pvCorrespondingType :: ExprRef def
  }
Lens.makeLenses ''PosVal

posValExprRefs :: Lens' (PosVal def) (ExprRef def)
posValExprRefs = pvCorrespondingType

type RefRigidityData def = (Last (PosVal def), Last PosType)

data Rigidity def = Rigidity
  { _rCounts :: PositionCounts
  , _rRefs ::
    OR.RefMap (TagExpr def) (RefRigidityData def)
  }
Lens.makeLenses ''Rigidity

rigidityExprRefs :: Lens.Traversal' (Rigidity def) (ExprRef def)
rigidityExprRefs f (Rigidity counts refs) =
  Rigidity counts
  <$> OR.unsafeRefMapItems
      ( bitraverse f
        ( bitraverse
          ((lastVal . posValExprRefs) f)
          ((lastVal . posTypeExprRefs) f)
        )
      ) refs

lastVal :: Lens.Traversal (Last a) (Last b) a b
lastVal = Lens.unwrapped . Lens._Just
