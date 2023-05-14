module TypeDef (
  TypeDef (..),
  TypeDefMap,
) where

import Foreword

import Data.Data (Data)

data TypeDef = TypeDef
  deriving stock (Eq, Show, Read, Data)

-- | A mapping of global names to 'TypeDef's.
type TypeDefMap = Map Text TypeDef
