{-# OPTIONS_GHC -fno-warn-orphans #-}

module Primer.OpenAPI (
  -- * Orphan instances
  -- $orphanInstances
) where

import Data.OpenApi (ToSchema)
import Primer.App (InitialApp)
import Primer.Database (Session, SessionName)

-- $orphanInstances
--
-- We define some OpenApi orphan instances in primer-service, to avoid
-- pulling in the openapi3 dependency into primer core. This is necessary to
-- build primer with ghcjs, because openapi3 transitively depends on network,
-- which ghcjs currently cannot build.

instance ToSchema SessionName
instance ToSchema Session
instance ToSchema InitialApp
