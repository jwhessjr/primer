-- | These functions allow you to create Core types easily, without having
-- to worry about generating unique IDs.
module Primer.Core.DSL.Type (
  tEmptyHole,
  tcon,
  tfun,
) where

import Foreword

import Control.Monad.Fresh (MonadFresh, fresh)
import Primer.Core.Meta (
  ID,
  Meta (..),
  TyConName,
 )
import Primer.Core.Type (
  Type,
  Type' (..),
 )

tEmptyHole :: MonadFresh ID m => m Type
tEmptyHole = TEmptyHole <$> meta

tcon :: MonadFresh ID m => TyConName -> m Type
tcon t = TCon <$> meta <*> pure t

tfun :: MonadFresh ID m => m Type -> m Type -> m Type
tfun a b = TFun <$> meta <*> a <*> b

meta :: MonadFresh ID m => m (Meta (Maybe a))
meta = meta' Nothing

meta' :: MonadFresh ID m => a -> m (Meta a)
meta' a = Meta <$> fresh <*> pure a <*> pure Nothing
