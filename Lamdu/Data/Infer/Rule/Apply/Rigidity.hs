{-# LANGUAGE RankNTypes #-}
module Lamdu.Data.Infer.Rule.Apply.Rigidity
  ( initial, firedKnownBody
  ) where

import Control.Lens.Operators
import Control.Monad.Trans.Class (MonadTrans(..))
import Data.Foldable (traverse_)
import Data.Maybe.Utils (unsafeUnjust)
import Data.Monoid (Monoid(..), Last(..))
import Lamdu.Data.Infer.Internal
import Lamdu.Data.Infer.Monad (Infer)
import Lamdu.Data.Infer.RefTags (ExprRef, TagExpr)
import Lamdu.Data.Infer.Rule.Types.Rigidity (Rigidity(..))
import qualified Control.Lens as Lens
import qualified Data.OpaqueRef as OR
import qualified Data.UnionFind.WithData as UFData
import qualified Lamdu.Data.Expression as Expr
import qualified Lamdu.Data.Infer.Monad as InferM
import qualified Lamdu.Data.Infer.Rule.Monad as RuleMonad
import qualified Lamdu.Data.Infer.Rule.Types as Rule
import qualified Lamdu.Data.Infer.Rule.Types.Rigidity as Rigidity
import qualified Lamdu.Data.Infer.Trigger as Trigger

emptyRefRigidityData :: Rigidity.RefRigidityData def
emptyRefRigidityData = mempty

refsInsertVal ::
  ExprRef def -> Rigidity.PosVal def ->
  OR.RefMap (TagExpr def) (Rigidity.RefRigidityData def) ->
  OR.RefMap (TagExpr def) (Rigidity.RefRigidityData def)
refsInsertVal ref val =
  Lens.at ref <>~
  Just (emptyRefRigidityData & Lens._1 .~ Last (Just val))

refsInsertType ::
  ExprRef def -> Rigidity.PosType ->
  OR.RefMap (TagExpr def) (Rigidity.RefRigidityData def) ->
  OR.RefMap (TagExpr def) (Rigidity.RefRigidityData def)
refsInsertType ref typ =
  Lens.at ref <>~
  Just (emptyRefRigidityData & Lens._2 .~ Last (Just typ))

initial ::
  TypedValue def -> (Rule.RuleRef def -> Infer def (), Rigidity def)
initial argTV =
  ( addTriggers
  , Rigidity
    { _rCounts = Rigidity.PositionCounts
      { Rigidity._pcOther = 1
      , Rigidity._pcRigid = 0
      , Rigidity._pcUnit = 0
      , Rigidity._pcRecord = 0
      }
    , _rRefs =
        mempty
        & refsInsertType argType
          Rigidity.PosType
          { Rigidity._ptKnowledge = Rigidity.TypeOther
          , Rigidity._ptCountedAs =
            Rigidity.CountedAsLevel Rigidity.Other
          }
        & refsInsertVal argVal
          Rigidity.PosVal
          { Rigidity._pvKnowledge = Rigidity.ValOther
          , Rigidity._pvCorrespondingType = argType
          }
    }
  )
  where
    TypedValue argVal argType = argTV
    addTriggers ruleRef = do
      Trigger.add Trigger.OnKnownBody ruleRef argVal
      Trigger.add Trigger.OnKnownBody ruleRef argType

findRef ::
  ExprRef def ->
  RuleMonad.RM (Rule.Apply def) def
  (ExprRef def, (Last (Rigidity.PosVal def), Last Rigidity.PosType))
findRef ref =
  Lens.zoom (Rule.aRigidity . Rigidity.rRefs)
  (OR.refMapUnmaintainedLookup find ref)
  <&> Lens._2 %~ unsafeUnjust "Apply.Rigidity.findRef"
  where
    find msg = lift . InferM.liftUFExprs . UFData.find msg

updateTypeRef ::
  Rigidity.CountedAs -> ExprRef def -> Rigidity.PosType ->
  RuleMonad.RM (Rule.Apply def) def ()
updateTypeRef newCountedAs typeRef posType = do
  Rule.aRigidity . Rigidity.rRefs %=
    refsInsertType typeRef
    (Rigidity.PosType newCountedAs typeKnowledge)
  Rule.aRigidity . Rigidity.rCounts %= updateCounts
  where
    updateCounts =
      (Rigidity.counterOf oldCountedAs -~ 1) .
      (Rigidity.counterOf newCountedAs +~ 1)
    Rigidity.PosType oldCountedAs typeKnowledge = posType

valKnowledgeOfBody :: Expr.Body a b -> Rigidity.ValKnowledge
valKnowledgeOfBody body =
  case body of
  Expr.BodyLeaf (Expr.GetVariable Expr.ParameterRef {}) ->
    Rigidity.ValGetParameter
  Expr.BodyRecord (Expr.Record Expr.KVal _) ->
    Rigidity.ValRecord
    -- TODO: Recursive triggers
  Expr.BodyRecord (Expr.Record Expr.KType _) -> Rigidity.ValOther
  Expr.BodyLeaf {} -> Rigidity.ValOther
  Expr.BodyGetField {} -> Rigidity.ValOther
  Expr.BodyApply {} -> Rigidity.ValOther
  Expr.BodyLam {} -> Rigidity.ValOther

knownBodyForVal ::
  ExprRef def -> Expr.Body a b ->
  Rigidity.PosVal def ->
  RuleMonad.RM (Rule.Apply def) def ()
knownBodyForVal valRef body posVal = do
  (typeRef, (_, Last mPosType)) <- findRef rawTypeRef
  let
    posType =
      unsafeUnjust "Rigidity: corresponding type of val not found"
      mPosType
    typeKnowledge = posType ^. Rigidity.ptKnowledge
    newValKnowledge = valKnowledgeOfBody body
    newCountedAs = Rigidity.countedAs typeKnowledge newValKnowledge
  Rule.aRigidity . Rigidity.rRefs %=
    refsInsertVal valRef Rigidity.PosVal
    { Rigidity._pvKnowledge = newValKnowledge
    , Rigidity._pvCorrespondingType = typeRef
    }
  updateTypeRef newCountedAs typeRef posType
  return ()
  where
    Rigidity.PosVal _ rawTypeRef = posVal

typeKnowledgeOfBody :: Expr.Body a b -> Rigidity.TypeKnowledge
typeKnowledgeOfBody body =
  case body of
  Expr.BodyLeaf leaf ->
    case leaf of
    Expr.GetVariable Expr.ParameterRef {} -> Rigidity.TypeType
    Expr.Type -> Rigidity.TypeType
    Expr.IntegerType -> Rigidity.TypeOther
    Expr.GetVariable Expr.DefinitionRef {} -> Rigidity.TypeOther
    Expr.Hole -> Rigidity.TypeOther
    Expr.TagType -> Rigidity.TypeOther
    Expr.LiteralInteger {} -> error "Invalid type (literal int)"
    Expr.Tag {} -> error "Invalid type (tag)"
  Expr.BodyRecord (Expr.Record Expr.KType _) ->
    -- TODO: Recursive triggers:
    Rigidity.TypeOther
  Expr.BodyGetField {} -> Rigidity.TypeOther
  Expr.BodyApply {} -> Rigidity.TypeOther
  Expr.BodyLam (Expr.Lam Expr.KType _ _ _) -> Rigidity.TypeOther
  Expr.BodyRecord (Expr.Record Expr.KVal _) -> error "Invalid type (record val)"
  Expr.BodyLam (Expr.Lam Expr.KVal _ _ _) -> error "Invalid type (lambda val)"

knownBodyForType ::
  ExprRef def -> Expr.Body a b ->
  Rigidity.PosType ->
  RuleMonad.RM (Rule.Apply def) def ()
knownBodyForType typeRef body posType =
  updateTypeRef newCountedAs typeRef $
  posType & Rigidity.ptKnowledge .~ newTypeKnowledge
  where
    newCountedAs =
      case (oldCountedAs, newTypeKnowledge) of
      (Rigidity.CountedAsLevel Rigidity.Rigid, Rigidity.TypeOther) ->
        oldCountedAs
      (_, Rigidity.TypeRecord) -> Rigidity.CountedAsRecord
      (_, Rigidity.TypeOther) -> Rigidity.CountedAsLevel Rigidity.Other
      (_, Rigidity.TypeUnit) -> Rigidity.CountedAsLevel Rigidity.Unit
      (_, Rigidity.TypeType) -> Rigidity.CountedAsLevel Rigidity.Rigid
    newTypeKnowledge = typeKnowledgeOfBody body
    Rigidity.PosType oldCountedAs _ = posType

firedKnownBody ::
  ExprRef def -> Expr.Body a b ->
  RuleMonad.RM (Rule.Apply def) def ()
firedKnownBody rawSrcRef body = do
  (srcRef, (Last mPosVal, Last mPosType)) <- findRef rawSrcRef
  traverse_ (knownBodyForType srcRef body) mPosType
  traverse_ (knownBodyForVal srcRef body) mPosVal
