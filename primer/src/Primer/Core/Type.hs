module Primer.Core.Type (
  Type (..),
  Kind (..),
) where

import Foreword

import Data.Data (Data)

data Type
  = TEmptyHole
  | TFun Type Type
  | TApp Type Type
  deriving stock (Eq, Show, Read, Data, Generic)

-- | Core kinds.
data Kind = KHole | KType | KFun Kind Kind
  deriving stock (Eq, Show, Read, Data, Generic)
