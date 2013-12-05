{-# LANGUAGE EmptyDataDecls #-}
module Lamdu.Data.Infer.RefTags
  ( TagExpr, ExprRef
  , TagRule, RuleRef
  , TagParam, ParamRef
  ) where

import qualified Data.OpaqueRef as OR

data TagExpr def
data TagRule def
data TagParam def

instance Show (TagExpr def) where show _ = "expr"
instance Show (TagRule def) where show _ = "rule"
instance Show (TagParam def) where show _ = "param"

type ExprRef def = OR.Ref (TagExpr def)
type RuleRef def = OR.Ref (TagRule def)
type ParamRef def = OR.Ref (TagParam def)
