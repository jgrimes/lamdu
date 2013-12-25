-- TODO: Remove this module, use trivial UF directly
{-# LANGUAGE TemplateHaskell #-}
module Lamdu.Data.Infer.GuidAliases
  ( GuidAliases, getRep, unify, empty, guidOfRep, find, hasGuid
  ) where

import Control.Applicative ((<$>), Applicative(..))
import Control.Lens.Operators
import Control.Monad (when)
import Control.Monad.Trans.State (StateT)
import Control.MonadA (MonadA)
import Data.Binary (Binary(..))
import Data.Map (Map)
import Data.Store.Guid (Guid)
import Data.UnionFind.WithData (UFData)
import Lamdu.Data.Infer.RefTags (ParamRef, TagParam)
import qualified Control.Lens as Lens
import qualified Data.Map as Map
import qualified Data.UnionFind.WithData as UFData

data GuidAliases def = GuidAliases
  -- TODO: Replace with a data-less union-find (no Guids)
  { _gaUF :: UFData (TagParam def) Guid -- Representative Guid
  , _gaGuidRefs :: Map Guid (ParamRef def)
  }
Lens.makeLenses ''GuidAliases

instance Binary (GuidAliases def) where
  get = GuidAliases <$> get <*> get
  put (GuidAliases x y) = put x >> put y

empty :: GuidAliases def
empty = GuidAliases
  { _gaUF = UFData.empty
  , _gaGuidRefs = Map.empty
  }

find :: MonadA m => ParamRef def -> StateT (GuidAliases def) m (ParamRef def)
find = Lens.zoom gaUF . UFData.find

getRep :: MonadA m => Guid -> StateT (GuidAliases def) m (ParamRef def)
getRep guid = do
  mRef <- Lens.use $ gaGuidRefs . Lens.at guid
  case mRef of
    Just ref -> do
      rep <- find ref
      when (rep /= ref) $ gaGuidRefs . Lens.at guid .= Just rep
      return rep
    Nothing -> do
      rep <- Lens.zoom gaUF $ UFData.fresh guid
      gaGuidRefs . Lens.at guid .= Just rep
      return rep

hasGuid :: Guid -> GuidAliases def -> Bool
hasGuid guid aliases = Map.member guid (aliases ^. gaGuidRefs)

unify :: MonadA m => ParamRef def -> ParamRef def -> StateT (GuidAliases def) m (ParamRef def)
unify xRep yRep = do
  Lens.zoom gaUF $ do
    (rep, result) <- UFData.unifyRefs xRep yRep
    case result of
      UFData.UnifyRefsUnified _ yRepGuid -> UFData.writeRep rep yRepGuid
      UFData.UnifyRefsAlreadyUnified -> return ()
    return rep

guidOfRep :: ParamRef def -> GuidAliases def -> Guid
guidOfRep rep guidAliases = UFData.readRep rep (guidAliases ^. gaUF)
