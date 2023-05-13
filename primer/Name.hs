module Name (
  Name (),
  unName,
  unsafeMkName,
) where

import Foreword

import Data.Data (Data)

-- | This module contains the type of names in the Core AST.

-- | Names are currently just wrappers around Text - in the future we may do
-- fancier things to provide better scoping guarantees etc.
newtype Name = Name {unName :: Text}
  deriving stock (Eq, Ord, Generic, Data)
  deriving newtype (Show, Read, IsString)

-- | Construct a name from a Text. This is called unsafe because there are no
-- guarantees about whether the name refers to anything that is in scope.
unsafeMkName :: Text -> Name
unsafeMkName = Name
