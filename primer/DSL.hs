-- | These functions allow you to create Core expressions easily, without having
-- to worry about generating unique IDs.
module DSL (
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

import Core (
  Bind' (..),
  CaseBranch,
  CaseBranch' (..),
  Expr,
  Expr' (..),
  ID,
  LVarName,
  Meta (..),
  TyConName,
  Type,
  Type' (..),
  TypeCache,
  ValConName,
 )
import Fresh (MonadFresh, fresh)

newtype S a = S {unS :: State ID a}
  deriving newtype (Functor, Applicative, Monad)

instance MonadFresh ID S where
  fresh = S $ do
    i <- get
    put (i + 1)
    pure i

-- | Evaluate a DSL expression with a starting ID of 0, producing an
-- @a@ and the next available fresh 'ID'.
create :: S a -> (a, ID)
create = flip runState 0 . unS

-- | As 'create', but drop the 'ID'.
create' :: S a -> a
create' = fst . create

meta :: MonadFresh ID m => m (Meta (Maybe a))
meta = meta' Nothing

meta' :: MonadFresh ID m => a -> m (Meta a)
meta' a = Meta <$> fresh <*> pure a <*> pure Nothing

tEmptyHole :: MonadFresh ID m => m Type
tEmptyHole = TEmptyHole <$> meta

tcon :: MonadFresh ID m => TyConName -> m Type
tcon t = TCon <$> meta <*> pure t

tfun :: MonadFresh ID m => m Type -> m Type -> m Type
tfun a b = TFun <$> meta <*> a <*> b

emptyHole :: MonadFresh ID m => m Expr
emptyHole = EmptyHole <$> meta

ann :: MonadFresh ID m => m Expr -> m Type -> m Expr
ann e t = Ann <$> meta <*> e <*> t

branch :: MonadFresh ID m => ValConName -> [(LVarName, Maybe TypeCache)] -> m Expr -> m CaseBranch
branch c vs e = CaseBranch c <$> mapM binding vs <*> e
  where
    binding (name, ty) = Bind <$> meta' ty <*> pure name
