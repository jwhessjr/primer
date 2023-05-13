module KindError (KindError (..)) where

import Foreword

import Type (Kind)

data KindError
  = UnknownTypeConstructor Text
  | InconsistentKinds Kind Kind
  | KindDoesNotMatchArrow Kind
  | -- | We currently cannot typecheck a let inside a type,
    -- they should only transiently appear in evaluation, as explicit substitutions.
    TLetUnsupported
  deriving stock (Eq, Show, Read)
