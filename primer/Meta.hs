module Meta (
  HasID (..),
  getID,
  ID (ID),
  ModuleName (ModuleName, unModuleName),
  GlobalNameKind (..),
  GlobalName (GlobalName, qualifiedModule, baseName),
  qualifyName,
  TyConName,
  GVarName,
  Meta (Meta),
) where

import Foreword

import Data.Data (Data)
import Data.Generics.Product
import Data.Generics.Uniplate.Data ()
import Data.Generics.Uniplate.Zipper (Zipper, hole, replaceHole)
import Name (Name)
import Optics (
  Lens',
  equality',
  lens,
  set,
  view,
 )

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

data Meta = Meta ID
  deriving stock (Generic, Eq, Show, Read, Data)

newtype ModuleName = ModuleName {unModuleName :: NonEmpty Name}
  deriving stock (Eq, Ord, Show, Read, Data, Generic)

-- | Tags for 'GlobalName'
data GlobalNameKind
  = ATyCon
  | ADefName

-- | Global names are fully qualified with a module name.
-- They are tagged with what sort of name they are.
data GlobalName (k :: GlobalNameKind) = GlobalName
  { qualifiedModule :: ModuleName
  , baseName :: Name
  }
  deriving stock (Eq, Ord, Generic, Data, Show, Read)

qualifyName :: ModuleName -> Name -> GlobalName k
qualifyName = GlobalName

type TyConName = GlobalName 'ATyCon
type GVarName = GlobalName 'ADefName

-- | A reference to a variable.
data TmVarRef
  = GlobalVarRef GVarName
  deriving stock (Eq, Show, Read, Data, Generic)

-- | A class for types which have an ID.
-- This makes it easier to change the underlying metadata representation without
-- breaking code that needs to work with IDs, because they use this class
-- instead of hardcoding paths to IDs or using chained 'HasType' instances,
-- which can lead to ambiguity errors.
class HasID a where
  _id :: Lens' a ID

instance HasID ID where
  _id = equality'

instance HasID Meta where
  _id = position @1

-- This instance is used in 'Zipper', but it would be an orphan if we defined it there.
instance HasID a => HasID (Zipper a a) where
  _id = lens getter setter
    where
      getter = view _id . hole
      setter z i =
        let t = hole z
         in replaceHole (set _id i t) z

-- | Get the ID of the given expression or type
getID :: HasID a => a -> ID
getID = view _id
