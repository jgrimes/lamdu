{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}
module Lamdu.Data.Infer.Deref
  ( M, expr, entireExpr, deref
  , toInferError
  , DerefedTV(..), dValue, dType, dScope, dTV
  , Error(..)
  , RefData.Restriction(..), ExprRef
  ) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Lens.Operators
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.State (StateT)
import Data.Binary (Binary(..))
import Data.Derive.Binary (makeBinary)
import Data.DeriveTH (derive)
import Data.Function.Decycle (decycle)
import Data.Traversable (traverse)
import Data.Typeable (Typeable)
import Lamdu.Data.Infer.Context (Context)
import Lamdu.Data.Infer.RefTags (ExprRef, TagParam)
import Lamdu.Data.Infer.TypedValue (TypedValue, tvVal, tvType)
import qualified Control.Lens as Lens
import qualified Data.OpaqueRef as OR
import qualified Data.UnionFind.WithData as UFData
import qualified Lamdu.Data.Expr as Expr
import qualified Lamdu.Data.Infer.Context as Context
import qualified Lamdu.Data.Infer.Monad as InferM
import qualified Lamdu.Data.Infer.RefData as RefData

data Error def = InfiniteExpr (ExprRef def)
  deriving (Show, Eq, Ord)

-- TODO: Makes little sense that we have loaded-def but "unloaded"
-- Guid. OTOH, if we leave the loaded guid here, we don't actually
-- deref any guids anywhere!
type Expr def = RefData.LoadedExpr def (ExprRef def)

data DerefedTV def = DerefedTV
  { _dValue :: Expr def
  , _dType :: Expr def
  , _dScope :: OR.RefMap (TagParam def) (Expr def) -- TODO: Make a separate derefScope action instead of this
  , _dTV :: TypedValue def
  } deriving (Typeable)
Lens.makeLenses ''DerefedTV
derive makeBinary ''DerefedTV

type M def = StateT (Context def) (Either (Error def))
mError :: Error def -> M def a
mError = lift . Left

-- canonicalGuid :: MonadA m => ParamRef def -> StateT (GuidAliases def) m Guid
-- canonicalGuid storedGuidsOfRefs guidRef = do
--   guidRep <- GuidAliases.find guidRef
--   storedExistingGuids <- do
--     aliases <- State.get
--     return $ filter ((`GuidAliases.hasGuid` aliases) . snd) storedGuidsOfRefs
--   storedGuidsOfReps <-
--     storedExistingGuids
--     & Lens.traverse . Lens._1 %%~ GuidAliases.find
--   case lookup guidRep storedGuidsOfReps of
--     Nothing -> State.gets (GuidAliases.guidOfRep guidRep)
--     Just storedGuid -> return storedGuid

deref :: ExprRef def -> M def (Expr def)
deref =
  decycle go
  where
    go Nothing ref = mError $ InfiniteExpr ref
    go (Just recurse) ref = do
      refData <- Lens.zoom Context.ufExprs (UFData.read ref)
      refData ^. RefData.rdBody
        & Lens.traverse %%~ recurse
        <&> (`Expr.Expr` ref)

derefScope ::
  OR.RefMap (TagParam def) (ExprRef def) ->
  M def (OR.RefMap (TagParam def) (Expr def))
derefScope =
  fmap OR.refMapFromList . traverse each . (^@.. Lens.itraversed)
  where
    each (paramRef, ref) = do
      typeExpr <- deref ref
      return (paramRef, typeExpr)

expr ::
  Expr.Expr ldef par (TypedValue def, a) ->
  M def (Expr.Expr ldef par (M def (DerefedTV def), a))
expr (Expr.Expr storedBody (tv, pl)) =
  storedBody
    & Lens.traverse %%~ expr
    <&> (`Expr.Expr` (derefTV, pl))
  where
    derefTV = do
      scope <-
        tv ^. tvVal
        & Lens.zoom Context.ufExprs . UFData.read
        <&> (^. RefData.rdScope)
      DerefedTV
        <$> deref (tv ^. tvVal)
        <*> deref (tv ^. tvType)
        <*> derefScope (scope ^. RefData.scopeMap)
        <*> pure tv

entireExpr ::
  Expr.Expr ldef par (TypedValue def, a) ->
  M def (Expr.Expr ldef par (DerefedTV def, a))
entireExpr = (>>= Lens.sequenceOf (Lens.traverse . Lens._1)) . expr
------- Lifted errors:

toInferError :: Error def -> InferM.Error def
toInferError (InfiniteExpr ref) = InferM.InfiniteExpr ref
