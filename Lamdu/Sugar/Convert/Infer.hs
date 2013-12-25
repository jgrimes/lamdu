{-# LANGUAGE TemplateHaskell, DeriveFunctor, ConstraintKinds #-}
module Lamdu.Sugar.Convert.Infer
  ( ExpressionSetter

  , isPolymorphicFunc
  , inferAddImplicits
  , InferredWithImplicits(..)

  , iwiBaseInferContext, iwiInferContext
  , iwiStructureInferContext
  , iwiExpr, iwiBaseExpr

  -- TODO: These don't belong here:
  -- Type-check an expression into an ordinary Inferred Expression,
  -- short-circuit on error:
  , load, memoInfer, memoInferAt

  , exprInferred
  , exprStored
  , exprGuid
  , exprStoredGuid
  , exprData

  , plIRef
  , exprIRef
  , replaceWith
  ) where

import Control.Applicative ((<$>))
import Control.Lens (Lens')
import Control.Lens.Operators
import Control.Monad (void, (<=<))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Either (EitherT(..), mapEitherT)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Trans.State (StateT(..), mapStateT, evalStateT)
import Control.MonadA (MonadA)
import Data.Binary (Binary)
import Data.Cache (Cache)
import Data.Maybe.Utils (unsafeUnjust)
import Data.Store.Guid (Guid)
import Data.Store.IRef (Tag)
import Data.Typeable (Typeable)
import Lamdu.Data.Expr.IRef (DefIM)
import Lamdu.Data.Infer.Deref (DerefedTV)
import Lamdu.Data.Infer.Load (Loader(..))
import Lamdu.Sugar.Types.Internal
import System.Random (RandomGen)
import qualified Control.Lens as Lens
import qualified Control.Monad.Trans.State as State
import qualified Data.Cache as Cache
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Lamdu.Data.Definition as Definition
import qualified Lamdu.Data.Expr as Expr
import qualified Lamdu.Data.Expr.IRef as ExprIRef
import qualified Lamdu.Data.Expr.Load as Load
import qualified Lamdu.Data.Expr.Utils as ExprUtil
import qualified Lamdu.Data.Infer as Infer
import qualified Lamdu.Data.Infer.Deref as InferDeref
import qualified Lamdu.Data.Infer.ImplicitVariables as ImplicitVariables
import qualified Lamdu.Data.Infer.Load as InferLoad
import qualified Lamdu.Data.Infer.Structure as Structure
import qualified Lamdu.Sugar.Types as Sugar

type ExpressionSetter def = Expr.Expr def Guid () -> Expr.Expr def Guid ()

loader :: MonadA m => Loader (DefIM m) (T m)
loader =
  Loader
  { loadDefType =
    fmap void . ExprIRef.readExpr . (^. Definition.bodyType) <=<
    Transaction.readIRef
  }

load ::
  ( Binary a, Typeable a
  , MonadA m, Typeable m
  ) =>
  ExprIRef.ExprM m a ->
  StateT (InferContext m) (MaybeT (CT m)) (LoadedExpr m a)
load expr = do
  loaded <-
    InferLoad.load loader expr
    & Lens.zoom icContext
    & mapStateT (eitherTToMaybeT . mapEitherT lift)
  icAugment loaded
  return loaded

memoBy ::
  (Cache.Key k, Binary v, Typeable v, MonadA m) =>
  Cache.FuncId -> k -> m v -> StateT Cache m v
memoBy funcId k act = Cache.memoS funcId (const act) k

pureMemoBy ::
  (Cache.Key k, Binary v, Typeable v, MonadA m) =>
  Cache.FuncId -> k -> v -> StateT Cache m v
pureMemoBy funcId k = memoBy funcId k . return

eitherToMaybe :: Either l a -> Maybe a
eitherToMaybe = either (const Nothing) Just

eitherTToMaybeT :: Functor m => EitherT l m a -> MaybeT m a
eitherTToMaybeT = (MaybeT . fmap eitherToMaybe . runEitherT)

memoInferAt ::
  (Typeable a, Binary a, Typeable m, MonadA m) =>
  Infer.TypedValue (DefIM m) ->
  LoadedExpr m a ->
  StateT (InferContext m) (MaybeT (CT m))
  (LoadedExpr m (InferDeref.DerefedTV (DefIM m), a))
memoInferAt tv expr = do
  -- TV uniquely identifies the position we're inferring to (stvScope
  -- is redundant to it):
  icAugment tv
  memoInferH $ Infer.inferAt tv expr

memoInfer ::
  (Typeable a, Binary a, Typeable m, MonadA m) =>
  Infer.Scope (DefIM m) -> LoadedExpr m a ->
  StateT (InferContext m) (MaybeT (CT m))
  (LoadedExpr m (InferDeref.DerefedTV (DefIM m), a))
memoInfer scope expr = memoInferH $ Infer.infer scope expr

memoInferH ::
  ( Typeable m, MonadA m
  , Typeable a, Binary a
  ) =>
  Infer.M (DefIM m) (LoadedExpr m (Infer.TypedValue (DefIM m), a)) ->
  StateT (InferContext m) (MaybeT (CT m))
  (LoadedExpr m (DerefedTV (DefIM m), a))
memoInferH infer = do
  k <- Lens.use icHashKey
  Lens.zoom icContext .
    mapStateT (MaybeT . pureMemoBy "memoInfer" k . eitherToMaybe) $
    mapStateT (Lens._Left %~ InferDeref.toInferError) . InferDeref.entireExpr =<<
    infer

inferWithVariables ::
  ( Show gen
  , RandomGen gen
  , MonadA m
  , Binary a
  , Typeable m
  , Typeable a
  ) =>
  gen -> DefIM m -> LoadedExpr m a ->
  Infer.Context (DefIM m) ->
  Infer.TypedValue (DefIM m) ->
  T m
  ( ( Infer.Context (DefIM m)
    , LoadedExpr m (DerefedTV (DefIM m), a)
    )
  , ( Infer.Context (DefIM m)
    , Infer.Context (DefIM m)
    , LoadedExpr m (DerefedTV (DefIM m), ImplicitVariables.Payload a)
    )
  )
inferWithVariables gen def loaded initialContext node =
  (`evalStateT` initialContext) $ do
    exprInferred <- assertSuccess $ Infer.inferAt node loaded
    expr <- assertSuccess $ InferDeref.entireExpr exprInferred
    -- TV should uniquely identify the scope of that same point
    -- (Within the context):
    baseContext <- State.get

    assertSuccess $ Structure.add exprInferred
    withStructureContext <- State.get

    wvExpr <-
      assertSuccess . InferDeref.entireExpr =<<
      assertSuccess (ImplicitVariables.add gen def exprInferred)
    wvContext <- State.get

    return
      ( (baseContext, expr)
      , (withStructureContext, wvContext, wvExpr)
      )

assertSuccess :: (Show e, Monad m) => StateT s (Either e) a -> StateT s m a
assertSuccess x = mapStateT (either (error . show) return) x

data InferredWithImplicits m a = InferredWithImplicits
  { _iwiInferContext :: InferContext m
  , _iwiStructureInferContext :: InferContext m
  , _iwiExpr :: LoadedExpr m (Sugar.InputPayloadP (Inferred m) (Maybe (Stored m)) a)
  -- Prior to adding variables
  , _iwiBaseInferContext :: InferContext m
  , _iwiBaseExpr :: LoadedExpr m (Sugar.InputPayloadP (Inferred m) (Stored m) a)
  }
Lens.makeLenses ''InferredWithImplicits

inferAddImplicits ::
  (Show gen, RandomGen gen, MonadA m, Typeable m) =>
  gen ->
  DefIM m ->
  ExprIRef.ExprM m (Load.ExprPropertyClosure (Tag m)) ->
  InferContext m -> Infer.TypedValue (DefIM m) ->
  CT m (InferredWithImplicits m ())
inferAddImplicits gen def lExpr inferContext node = do
  -- TODO: Propagate errors?
  (loaded, loadedContext) <-
    fmap (unsafeUnjust "inferAddImplicits load failed") . runMaybeT .
    (`runStateT` inferContext) $ load lExpr
  let
    key =
      Cache.bsOfKey
      ( show gen, def, loaded, node
      , (loadedContext ^. icHashKey)
      )
  ((baseContext, expr), (withStructureContext, wvContext, wvExpr)) <-
    inferWithVariables gen def loaded (loadedContext ^. icContext) node
    & memoBy "inferAddImplicits" key
  let newContext x ctx = InferContext ctx $ Cache.bsOfKey (key, x)
  return InferredWithImplicits
    { _iwiBaseInferContext = newContext "base" baseContext
    , _iwiBaseExpr = mkStoredPayload <$> expr
    , _iwiStructureInferContext = newContext "structure" withStructureContext
    , _iwiInferContext = newContext "variables" wvContext
    , _iwiExpr = mkWVPayload <$> wvExpr
    }
  where
    mkStoredPayload (iwc, propClosure) =
      Sugar.InputPayload (ExprIRef.epGuid prop) iwc prop ()
      where
        prop = Load.exprPropertyOfClosure propClosure
    mkWVPayload (iwc, ImplicitVariables.AutoGen guid) =
      Sugar.InputPayload guid iwc Nothing ()
    mkWVPayload (iwc, ImplicitVariables.Stored propClosure) =
      mkStoredPayload (iwc, propClosure)
      & Sugar.ipStored %~ Just

isPolymorphicFunc :: Sugar.InputPayload m a -> Bool
isPolymorphicFunc funcPl =
  maybe False ExprUtil.isDependentPi $
  funcPl ^? Sugar.ipInferred . Lens._Just . InferDeref.dType

exprGuid ::
  Lens' (Expr.Expr def Guid (Sugar.InputPayloadP inferred stored a)) Guid
exprGuid = Expr.ePayload . Sugar.ipGuid

exprStored ::
  Lens' (Expr.Expr def Guid (Sugar.InputPayloadP inferred stored a)) stored
exprStored = Expr.ePayload . Sugar.ipStored

exprInferred ::
  Lens' (Expr.Expr def Guid (Sugar.InputPayloadP inferred stored a)) inferred
exprInferred = Expr.ePayload . Sugar.ipInferred

exprData ::
  Lens' (Expr.Expr def Guid (Sugar.InputPayloadP inferred stored a)) a
exprData = Expr.ePayload . Sugar.ipData

-- TODO: Move to ...?
plIRef ::
  Lens.Traversal' (Sugar.InputPayloadP i (Maybe (Stored m)) a) (ExprIRef.ExprIM m)
plIRef = Sugar.ipStored . Lens._Just . Property.pVal

exprStoredGuid ::
  Lens.Fold
  (Expr.Expr def Guid (Sugar.InputPayloadP i (Maybe (Stored m)) a)) Guid
exprStoredGuid = exprIRef . Lens.to ExprIRef.exprGuid

replaceWith :: MonadA m => Stored m -> Stored m -> T m Guid
replaceWith parentP replacerP = do
  Property.set parentP replacerI
  return $ ExprIRef.exprGuid replacerI
  where
    replacerI = Property.value replacerP

exprIRef ::
  Lens.Traversal'
  (Expr.Expr def Guid (Sugar.InputPayloadP i (Maybe (Stored m)) a))
  (ExprIRef.ExprIM m)
exprIRef = exprStored . Lens._Just . Property.pVal
