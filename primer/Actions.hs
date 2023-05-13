module Actions (
  Action (..),
  Movement (..),
) where

import Foreword

import Movement (Movement (..))

-- We split this module to increase parallelism in our build.
-- This module does not depend on much, but takes a long time
-- to build because of
-- https://gitlab.haskell.org/ghc/ghc/-/issues/5642

-- | Core actions.
--  These describe edits to the core AST.
--  Some of them take Text arguments rather than Name because they represent
--  untrusted input from the frontend.
data Action
  = SetCursor Int
  | Delete
  | ConstructArrowL
  deriving stock (Eq, Show, Read)
