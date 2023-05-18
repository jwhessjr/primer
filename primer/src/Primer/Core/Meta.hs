module Primer.Core.Meta (
  ID (ID),
  ValConName,
  GVarName,
  Meta (Meta),
  trivialMeta,
  _type,
) where

import Foreword

import Data.Aeson (Value)
import Data.Data (Data)
import Data.Generics.Product
import Data.Generics.Uniplate.Data ()
import Optics (
  Lens,
 )
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

data Meta a = Meta ID a (Maybe Value)
  deriving stock (Generic, Eq, Show, Read, Data, Functor)

-- | This lens is called 'type' because 'a' is most commonly a Type, but it will
-- work for any 'a'.
_type :: Lens (Meta a) (Meta b) a b
_type = position @2

trivialMeta :: ID -> Meta (Maybe a)
trivialMeta id = Meta id Nothing Nothing

type ValConName = Name
type GVarName = Name
