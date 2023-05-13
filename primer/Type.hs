module Type (
  Type,
  Type' (..),
  Kind (..),
  TypeMeta,
  _typeMeta,
  _typeMetaLens,
) where

import Foreword

import Data.Data (Data)
import Data.Generics.Product
import Meta (
  HasID (..),
  Meta,
  TyConName,
 )
import Optics (
  Lens',
  Traversal,
  (%),
 )

-- | Core types.
--  Type variables are currently represented as text, and we have no compile-time
--  checks on scoping. We may want to introduce de Bruijn indices or use
--  bound/unbound in the future.
type Type = Type' TypeMeta

-- | Type metadata. Each type is optionally annotated with a kind.
-- Currently we don't fill these in during typechecking.
type TypeMeta = Meta (Maybe Kind)

-- | NB: Be careful with equality -- it is on-the-nose, rather than up-to-alpha: see Subst:alphaEqTy
data Type' a
  = TEmptyHole a
  | TCon a TyConName
  | TFun a (Type' a) (Type' a)
  deriving stock (Eq, Show, Read, Data, Generic)

-- | A traversal over the metadata of a type
_typeMeta :: Traversal (Type' a) (Type' b) a b
_typeMeta = param @0

-- | A lens on to the metadata of a type.
-- Note that unlike '_typeMeta', this is shallow i.e. it does not recurse in to sub-expressions.
-- And for this reason, it cannot be type-changing.
_typeMetaLens :: Lens' (Type' a) a
_typeMetaLens = position @1

-- | Core kinds.
data Kind = KType
  deriving stock (Eq, Show, Read, Data, Generic)

instance HasID a => HasID (Type' a) where
  _id = position @1 % _id
