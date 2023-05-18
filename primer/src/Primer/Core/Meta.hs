module Primer.Core.Meta (
  ID (ID),
  ValConName,
  GVarName,
) where

import Foreword

import Data.Data (Data)
import Data.Generics.Uniplate.Data ()
import Primer.Name (Name)

-- | An identifier for an expression. Every node of the AST has an ID.
--
-- Note that we may remove the 'Ord', 'Enum', and/or 'Bounded'
-- instances in future implementations, so you should try not to rely
-- on them. (Internally, we rely on 'Ord' and 'Bounded', but that may
-- change in the future and is more or less not visible to external
-- consumers of this type.)
newtype ID = ID Int
  deriving stock (Eq, Data)
  deriving newtype (Show, Read, Num, Ord, Enum, Bounded)

type ValConName = Name
type GVarName = Name
