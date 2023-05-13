module KindError (KindError (..)) where

import Foreword

import Type (Kind)

data KindError
  = UnknownTypeConstructor Text
  | InconsistentKinds Kind Kind
  | KindDoesNotMatchArrow Kind
  deriving stock (Eq, Show, Read)
