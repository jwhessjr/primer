-- | These functions allow you to create Core expressions easily, without having
-- to worry about generating unique IDs.
module Primer.Core.DSL (
  emptyHole,
  branch,
  meta,
  meta',
) where

import Foreword

import Control.Monad.Fresh (MonadFresh)
import Primer.Core (
  Bind' (..),
  CaseBranch,
  CaseBranch' (..),
  Expr,
  Expr' (..),
  ID,
  LVarName,
  TypeCache,
  ValConName,
 )
import Primer.Core.DSL.Meta (meta, meta')

emptyHole :: MonadFresh ID m => m Expr
emptyHole = EmptyHole <$> meta

branch :: MonadFresh ID m => ValConName -> [(LVarName, Maybe TypeCache)] -> m Expr -> m CaseBranch
branch c vs e = CaseBranch c <$> mapM binding vs <*> e
  where
    binding (name, ty) = Bind <$> meta' ty <*> pure name
