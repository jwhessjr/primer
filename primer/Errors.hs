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
import Core (Expr)
import Movement (Movement)
import Type (Kind, Type')

data Error
  = UnknownTypeConstructor Text
  | InconsistentKinds Kind Kind
  | KindDoesNotMatchArrow Kind
  | InternalError Text
  | CannotSynthesiseType Expr
  | InconsistentTypes (Type' ()) (Type' ())
  | TypeDoesNotMatchArrow (Type' ())
  | CustomFailure
      Action
      -- ^ action that caused the error
      Text
      -- ^ the error message
  | InternalFailure Text
  | IDNotFound Int
  | MovementFailed (Int, Movement)
  | -- | Both actual and potential, eg renaming the lambda x to y in any of
    -- λx.y     the binder captures the existing y
    -- λx.λy.x  occurance gets captured by the inner binder
    -- λx.λy.y  this would be ok, but we are paranoid and bail out
    NameCapture
  | NoNodeSelection
  | NoDefSelected
  | DefNotFound Text
  | DefAlreadyExists Text
  | DefInUse Text
  | -- | Currently copy/paste is only exposed in the frontend via select
    --   channels, which should never go wrong. Consequently, this is an
    --   "internal error" which should never happen!
    --   If/when we expose it more broadly, we should refactor this to contain
    --   a descriptive ADT, rather than a string.
    CopyPasteError Text
  deriving stock (Eq, Show, Read)
