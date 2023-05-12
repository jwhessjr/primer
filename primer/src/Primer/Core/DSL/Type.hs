-- | These functions allow you to create Core types easily, without having
-- to worry about generating unique IDs.
module Primer.Core.DSL.Type (
  tEmptyHole,
  tcon,
  tforall,
  tfun,
  tapp,
  tvar,
) where

import Foreword

import Control.Monad.Fresh (MonadFresh, fresh)
import Primer.Core.Meta (
  ID,
  Meta (..),
  TyConName,
  TyVarName,
 )
import Primer.Core.Type (
  Kind,
  Type,
  Type' (..),
 )

tEmptyHole :: MonadFresh ID m => m Type
tEmptyHole = TEmptyHole <$> meta

tcon :: MonadFresh ID m => TyConName -> m Type
tcon t = TCon <$> meta <*> pure t

tforall :: MonadFresh ID m => TyVarName -> Kind -> m Type -> m Type
tforall v k t = TForall <$> meta <*> pure v <*> pure k <*> t

tfun :: MonadFresh ID m => m Type -> m Type -> m Type
tfun a b = TFun <$> meta <*> a <*> b

tapp :: MonadFresh ID m => m Type -> m Type -> m Type
tapp a b = TApp <$> meta <*> a <*> b

tvar :: MonadFresh ID m => TyVarName -> m Type
tvar v = TVar <$> meta <*> pure v

meta :: MonadFresh ID m => m (Meta (Maybe a))
meta = meta' Nothing

meta' :: MonadFresh ID m => a -> m (Meta a)
meta' a = Meta <$> fresh <*> pure a <*> pure Nothing
