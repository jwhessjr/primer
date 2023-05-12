-- | These functions allow you to create Core expressions easily, without having
-- to worry about generating unique IDs.
module Primer.Core.DSL (
  emptyHole,
  hole,
  ann,
  app,
  aPP,
  con,
  var,
  lam,
  lAM,
  let_,
  letrec,
  case_,
  branch,
  tEmptyHole,
  tcon,
  tforall,
  tfun,
  tapp,
  tvar,
  meta,
  meta',
  create,
  create',
  S,
  apps',
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
  TmVarRef (..),
  TyVarName,
  Type,
  TypeCache,
  ValConName,
 )
import Primer.Core.DSL.Meta (S, create, create', meta, meta')
import Primer.Core.DSL.Type (
  tEmptyHole,
  tapp,
  tcon,
  tforall,
  tfun,
  tvar,
 )

app :: MonadFresh ID m => m Expr -> m Expr -> m Expr
app e1 e2 = App <$> meta <*> e1 <*> e2

-- | `apps` for expressions and types
apps' :: MonadFresh ID m => m Expr -> [Either (m Expr) (m Type)] -> m Expr
apps' = foldl' app'
  where
    app' e (Left e') = e `app` e'
    app' e (Right t) = e `aPP` t

aPP :: MonadFresh ID m => m Expr -> m Type -> m Expr
aPP e t = APP <$> meta <*> e <*> t

hole :: MonadFresh ID m => m Expr -> m Expr
hole e = Hole <$> meta <*> e

emptyHole :: MonadFresh ID m => m Expr
emptyHole = EmptyHole <$> meta

ann :: MonadFresh ID m => m Expr -> m Type -> m Expr
ann e t = Ann <$> meta <*> e <*> t

con :: MonadFresh ID m => ValConName -> m Expr
con c = Con <$> meta <*> pure c

var :: MonadFresh ID m => TmVarRef -> m Expr
var v = Var <$> meta <*> pure v

lam :: MonadFresh ID m => LVarName -> m Expr -> m Expr
lam v e = Lam <$> meta <*> pure v <*> e

lAM :: MonadFresh ID m => TyVarName -> m Expr -> m Expr
lAM v e = LAM <$> meta <*> pure v <*> e

let_ :: MonadFresh ID m => LVarName -> m Expr -> m Expr -> m Expr
let_ v a b = Let <$> meta <*> pure v <*> a <*> b

letrec :: MonadFresh ID m => LVarName -> m Expr -> m Type -> m Expr -> m Expr
letrec v a tA b = Letrec <$> meta <*> pure v <*> a <*> tA <*> b

case_ :: MonadFresh ID m => m Expr -> [m CaseBranch] -> m Expr
case_ e brs = Case <$> meta <*> e <*> sequence brs

branch :: MonadFresh ID m => ValConName -> [(LVarName, Maybe TypeCache)] -> m Expr -> m CaseBranch
branch c vs e = CaseBranch c <$> mapM binding vs <*> e
  where
    binding (name, ty) = Bind <$> meta' ty <*> pure name
