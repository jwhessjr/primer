module Errors (
  Error (..),
  ) where

-- We split this module to increase parallelism in our build.
-- This module does not depend on much, but takes a long time
-- to build because of
-- https://gitlab.haskell.org/ghc/ghc/-/issues/5642
--
-- It is split from Actions because this depends on Expr

import Foreword

import Actions (Action)

data Error
  = UnknownTypeConstructor Text
  | InternalError Text
  | CustomFailure Action Text
  | InternalFailure Text
  | IDNotFound Int
  | NameCapture
  | NoNodeSelection
  | NoDefSelected
  | DefNotFound Text
  | DefAlreadyExists Text
  | DefInUse Text
  deriving stock (Eq, Show, Read)
