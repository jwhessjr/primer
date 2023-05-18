module Primer.Core.Meta (
  ID (ID),
  ModuleName (ModuleName, unModuleName),
  GlobalNameKind (..),
  GlobalName (GlobalName, qualifiedModule, baseName),
  TyConName,
  ValConName,
  GVarName,
  LocalNameKind (..),
  LocalName (LocalName, unLocalName),
  TmVarRef (..),
  LVarName,
  TyVarName,
  Value,
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

newtype ModuleName = ModuleName {unModuleName :: NonEmpty Name}
  deriving stock (Eq, Ord, Show, Read, Data, Generic)

-- | Tags for 'GlobalName'
data GlobalNameKind
  = ATyCon
  | AValCon
  | ADefName

-- | Global names are fully qualified with a module name.
-- They are tagged with what sort of name they are.
data GlobalName (k :: GlobalNameKind) = GlobalName
  { qualifiedModule :: ModuleName
  , baseName :: Name
  }
  deriving stock (Eq, Ord, Generic, Data, Show, Read)

type TyConName = GlobalName 'ATyCon
type ValConName = GlobalName 'AValCon
type GVarName = GlobalName 'ADefName

-- | Tags for 'LocalName'
data LocalNameKind
  = ATmVar
  | ATyVar

-- | A newtype wrapper around a 'Name', tracking that the name refers
-- to a local variable. The tag says which sort of variable (term or
-- type) this is.
newtype LocalName (k :: LocalNameKind) = LocalName {unLocalName :: Name}
  deriving stock (Eq, Ord, Show, Read, Data, Generic)
  deriving (IsString) via Name

type LVarName = LocalName 'ATmVar
type TyVarName = LocalName 'ATyVar

-- | A reference to a variable.
data TmVarRef
  = GlobalVarRef GVarName
  | LocalVarRef LVarName
  deriving stock (Eq, Show, Read, Data, Generic)
