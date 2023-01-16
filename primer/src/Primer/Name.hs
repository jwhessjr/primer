module Primer.Name (
  Name (),
  NameCounter,
  freshName,
  unName,
  unsafeMkName,
) where

import Foreword

import Control.Monad.Fresh (MonadFresh, fresh)
import Data.Char qualified as C
import Data.Data (Data)
import Data.Set qualified as S
import Data.String (String)
import Numeric.Natural (Natural)
import Primer.JSON

-- | This module contains the type of names in the Core AST.

-- | Names are currently just wrappers around Text - in the future we may do
-- fancier things to provide better scoping guarantees etc.
newtype Name = Name {unName :: Text}
  deriving stock (Eq, Ord, Generic, Data)
  deriving newtype (Show, IsString)
  deriving newtype (FromJSON, ToJSON, FromJSONKey, ToJSONKey)
  deriving anyclass (NFData)

-- | Construct a name from a Text. This is called unsafe because there are no
-- guarantees about whether the name refers to anything that is in scope.
unsafeMkName :: Text -> Name
unsafeMkName = Name

newtype NameCounter = NC Natural
  deriving stock (Eq, Show, Generic, Data)
  deriving newtype (Enum, FromJSON, ToJSON)
  deriving anyclass (NFData)

-- | Generate a new automatic name (distinct from all other names generated by
-- this function, from the MonadFresh class), and avoiding the given set of
-- names.
-- Due to the extra argument, this cannot be exposed as the 'fresh' method of
-- a MonadFresh Name class.
freshName :: MonadFresh NameCounter m => S.Set Name -> m Name
freshName avoid = go
  where
    go = do
      NC n <- fresh
      let s = Name $ toS $ genAlpha n
      if s `S.member` avoid
        then go
        else pure s

-- | Convert an integer into an alphanumeric string. Each integer has a unique
-- representation.
--
-- >>> map genAlpha [0, 1, 30, 31]
-- ["a", "b", "e1", "f1"]
--
-- Note: replace this use of `String`. See:
-- https://github.com/hackworthltd/primer/issues/149
genAlpha :: Natural -> String
genAlpha n =
  let c = C.chr $ fromInteger $ toInteger (97 + (n `mod` 26))
      m = n `div` 26
   in if m > 0 then c : show m else [c]
