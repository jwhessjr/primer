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
  (%),
 )
import Primer.Core.Meta (
  HasID (..),
  HasMetadata (_metadata),
  Meta,
  TyConName,
  TyVarName,
  Value,
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
  | THole a (Type' a)
  | TCon a TyConName
  | TFun a (Type' a) (Type' a)
  | TVar a TyVarName
  | TApp a (Type' a) (Type' a)
  | TForall a TyVarName Kind (Type' a)
  | -- | TLet is a let binding at the type level.
    -- It is currently only constructed automatically during evaluation -
    -- the student can't directly make it.
    TLet
      a
      TyVarName
      -- ^ bound variable
      (Type' a)
      -- ^ type the variable is bound to; the variable itself is not in scope, this is a non-recursive let
      (Type' a)
      -- ^ body of the let; binding scopes over this
  deriving stock (Eq, Show, Read, Data, Generic)

-- | A traversal over the metadata of a type
_typeMeta :: Traversal (Type' a) (Type' b) a b
_typeMeta = param @0

-- | Core kinds.
data Kind = KHole | KType | KFun Kind Kind
  deriving stock (Eq, Show, Read, Data, Generic)

instance HasID a => HasID (Type' a) where
  _id = position @1 % _id

instance HasMetadata (Type' TypeMeta) where
  _metadata = position @1 % typed @(Maybe Value)
