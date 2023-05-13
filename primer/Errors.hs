module Errors (ActionError (..)) where

-- We split this module to increase parallelism in our build.
-- This module does not depend on much, but takes a long time
-- to build because of
-- https://gitlab.haskell.org/ghc/ghc/-/issues/5642
--
-- It is split from Actions because this depends on Expr

import Foreword

import Actions (Action)
import Core (ID)
import Movement (Movement)
import TypeError (TypeError)

-- | Errors that may arise when applying an action
-- TODO: convert all CustomFailures to individual constructors
-- https://github.com/hackworthltd/primer/issues/8
data ActionError
  = CustomFailure
      Action
      -- ^ action that caused the error
      Text
      -- ^ the error message
  | InternalFailure Text
  | IDNotFound ID
  | MovementFailed (ID, Movement)
  | TypeError TypeError
  | -- | Both actual and potential, eg renaming the lambda x to y in any of
    -- λx.y     the binder captures the existing y
    -- λx.λy.x  occurance gets captured by the inner binder
    -- λx.λy.y  this would be ok, but we are paranoid and bail out
    NameCapture
  | NoNodeSelection
  deriving stock (Eq, Show, Read, Generic)

-- cannot remove generic here as is used in MonadNestedError
