-- | These functions allow you to create Core metadata easily, without having
-- to worry about generating unique IDs.
module Primer.Core.DSL.Meta (
  meta,
  meta',
) where

import Foreword

import Control.Monad.Fresh (MonadFresh, fresh)
import Primer.Core.Meta (
  ID,
  Meta (..),
 )

meta :: MonadFresh ID m => m (Meta (Maybe a))
meta = meta' Nothing

meta' :: MonadFresh ID m => a -> m (Meta a)
meta' a = Meta <$> fresh <*> pure a <*> pure Nothing
