module Primer.Core.Type (
  Type (..),
  Kind (..),
) where

import Prelude

data Type
  = TEmptyHole
  | TFun Type Type
  | TApp Type Type
  deriving stock (Eq, Show)

-- | Core kinds.
data Kind = KHole | KType | KFun Kind Kind
  deriving stock (Eq, Show)
