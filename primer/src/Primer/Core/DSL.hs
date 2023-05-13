-- | These functions allow you to create Core expressions easily, without having
-- to worry about generating unique IDs.
module Primer.Core.DSL (
  emptyHole,
  ann,
  branch,
  tEmptyHole,
  tcon,
  tfun,
  meta,
  meta',
  create,
  create',
  S,
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
  Type,
  TypeCache,
  ValConName,
 )
import Primer.Core.DSL.Meta (S, create, create', meta, meta')
import Primer.Core.DSL.Type (
  tEmptyHole,
  tcon,
  tfun,
 )

emptyHole :: MonadFresh ID m => m Expr
emptyHole = EmptyHole <$> meta

ann :: MonadFresh ID m => m Expr -> m Type -> m Expr
ann e t = Ann <$> meta <*> e <*> t

branch :: MonadFresh ID m => ValConName -> [(LVarName, Maybe TypeCache)] -> m Expr -> m CaseBranch
branch c vs e = CaseBranch c <$> mapM binding vs <*> e
  where
    binding (name, ty) = Bind <$> meta' ty <*> pure name
