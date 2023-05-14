module ProgAction (ProgAction (..)) where

-- We split this module to increase parallelism in our build.
-- This module does not depend on much, but takes a long time
-- to build because of
-- https://gitlab.haskell.org/ghc/ghc/-/issues/5642
--
-- It is split from Actions and ActionError
-- because this depends on ASTTypeDef

import Foreword

import Actions (Action)

-- | High level actions
-- These actions move around the whole program or modify definitions
data ProgAction
  = RenameDef Text Text
  | DeleteDef Text
  | BodyAction Text Int Action
  | SigAction Text Int Action
  deriving stock (Eq, Show, Read)
