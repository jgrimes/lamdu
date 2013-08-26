{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable, TemplateHaskell, OverloadedStrings #-}
module Lamdu.Sugar.Types.Internal
  ( StorePoint(..), T, CT
  , InferContext(..), icContext, icHashKey
  , initialInferContext
  , NoInferred(..), InferredWC
  , NoStored(..), Stored
  ) where

import Control.Monad.Trans.State (StateT)
import Data.Binary (Binary)
import Data.Cache (Cache)
import Data.Store.Transaction (Transaction)
import Data.Typeable (Typeable, Typeable1)
import Lamdu.Data.Expression.IRef (DefIM)
import Lamdu.Data.Infer.Deref (DerefedSTV)
import qualified Control.Lens as Lens
import qualified Data.Cache as Cache
import qualified Lamdu.Data.Expression.IRef as ExprIRef
import qualified Lamdu.Data.Infer as Infer
import qualified System.Random as Random

type T = Transaction
type CT m = StateT Cache (T m)

data NoInferred = NoInferred
type InferredWC m = DerefedSTV (DefIM m) -- TODO: Rename InferredWC

data NoStored = NoStored
type Stored m = ExprIRef.ExpressionProperty m

newtype StorePoint t = StorePoint { unStorePoint :: ExprIRef.ExpressionI t }
  deriving (Eq, Ord, Binary, Typeable)

data InferContext m = InferContext
  { _icContext :: Infer.Context (DefIM m)
  , _icHashKey :: Cache.KeyBS
    -- ^ icHashKey is a "compact" unique identifier of the icContext
    -- for cheaper memoization
  }
Lens.makeLenses ''InferContext

initialInferContext :: Typeable1 m => InferContext m
initialInferContext = InferContext (Infer.emptyContext (Random.mkStdGen 0)) ""
