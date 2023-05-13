module TypeError (TypeError (..)) where

import Foreword

import Core (Expr)
import KindError (KindError)
import Type (Type')

data TypeError
  = InternalError Text
  | CannotSynthesiseType Expr
  | InconsistentTypes (Type' ()) (Type' ())
  | TypeDoesNotMatchArrow (Type' ())
  | KindError KindError
  deriving stock (Eq, Show, Read)
