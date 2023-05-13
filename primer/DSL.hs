-- | These functions allow you to create Core expressions easily, without having
-- to worry about generating unique IDs.
module DSL (
  emptyHole,
  ann,
  tEmptyHole,
  tcon,
  tfun,
  create,
  S,
) where

import Foreword

import Core (
  Expr,
  Expr' (..),
  ID,
  Type,
  Type' (..),
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

tEmptyHole :: MonadFresh ID m => m Type
tEmptyHole = TEmptyHole <$> fresh

tcon :: MonadFresh ID m => Text -> m Type
tcon t = TCon <$> fresh <*> pure t

tfun :: MonadFresh ID m => m Type -> m Type -> m Type
tfun a b = TFun <$> fresh <*> a <*> b

emptyHole :: MonadFresh ID m => m Expr
emptyHole = EmptyHole <$> fresh

ann :: MonadFresh ID m => m Expr -> m Type -> m Expr
ann e t = Ann <$> fresh <*> e <*> t
