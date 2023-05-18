module Primer.Core.Type (
  Type,
  Type' (..),
  Kind (..),
  TypeMeta,
  _typeMeta,
) where

import Foreword

import Data.Data (Data)
import Data.Generics.Product
import Optics (
  Traversal,
 )
import Primer.Core.Meta (
  Meta,
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
  | TFun a (Type' a) (Type' a)
  | TApp a (Type' a) (Type' a)
  deriving stock (Eq, Show, Read, Data, Generic)

-- | A traversal over the metadata of a type
_typeMeta :: Traversal (Type' a) (Type' b) a b
_typeMeta = param @0

-- | Core kinds.
data Kind = KHole | KType | KFun Kind Kind
  deriving stock (Eq, Show, Read, Data, Generic)
