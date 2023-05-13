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
import Meta (GVarName)

-- | High level actions
-- These actions move around the whole program or modify definitions
data ProgAction
  = -- | Move the cursor to the definition with the given Name
    MoveToDef GVarName
  | -- | Rename the definition with the given (base) Name
    RenameDef GVarName Text
  | -- | Delete a new definition
    DeleteDef GVarName
  | -- | Execute a sequence of actions on the body of the definition
    BodyAction [Action]
  | -- | Execute a sequence of actions on the type annotation of the definition
    SigAction [Action]
  deriving stock (Eq, Show, Read)
