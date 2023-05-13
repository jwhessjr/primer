module KindError (KindError (..)) where

import Foreword

import Meta (TyConName)
import Type (Kind)

data KindError
  = UnknownTypeConstructor TyConName
  | InconsistentKinds Kind Kind
  | KindDoesNotMatchArrow Kind
  | -- | We currently cannot typecheck a let inside a type,
    -- they should only transiently appear in evaluation, as explicit substitutions.
    TLetUnsupported
  deriving stock (Eq, Show, Read)
