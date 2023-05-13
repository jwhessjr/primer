module Actions (
  Action (..),
  Movement (..),
  QualifiedText,
) where

import Foreword

import Movement (Movement (..))
import Meta (ID)

-- We split this module to increase parallelism in our build.
-- This module does not depend on much, but takes a long time
-- to build because of
-- https://gitlab.haskell.org/ghc/ghc/-/issues/5642

type QualifiedText = (NonEmpty Text, Text)

-- | Core actions.
--  These describe edits to the core AST.
--  Some of them take Text arguments rather than Name because they represent
--  untrusted input from the frontend.
data Action
  = SetCursor ID
  | -- | Move one step in some direction
    Move Movement
  | -- | Delete the expression under the cursor
    Delete
  | -- | Construct a function type around the type under the cursor.
    -- The type under the cursor is placed in the domain (left) position.
    ConstructArrowL
  deriving stock (Eq, Show, Read)
