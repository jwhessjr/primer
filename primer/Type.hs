module Type (
  Type (..),
  Kind (..),
  _typeMeta,
  _typeMetaLens,
) where

import Foreword

import Data.Data (Data)
import Meta (
  HasID (..),
 )
import Optics (
  Lens',
  Traversal',
  (%), traversalVL, lensVL,
 )

-- | NB: Be careful with equality -- it is on-the-nose, rather than up-to-alpha: see Subst:alphaEqTy
data Type
  = TEmptyHole Int
  | TCon Int Text
  | TFun Int Type Type
  deriving stock (Eq, Show, Read, Data, Generic)

-- | A traversal over the metadata of a type
_typeMeta :: Traversal' Type Int
_typeMeta = traversalVL go
  where
    go f = \case
      TEmptyHole i -> TEmptyHole <$> f i
      TCon i c -> TCon <$> f i <*> pure c
      TFun i s t -> TFun <$> f i <*> go f s <*> go f t

-- | A lens on to the metadata of a type.
-- Note that unlike '_typeMeta', this is shallow i.e. it does not recurse in to sub-expressions.
-- And for this reason, it cannot be type-changing.
_typeMetaLens :: Lens' Type Int
_typeMetaLens = lensVL $ \f -> \case
  TEmptyHole i -> TEmptyHole <$> f i
  TCon i c -> (`TCon` c) <$> f i
  TFun i s t -> (\j -> TFun j s t) <$> f i

-- | Core kinds.
data Kind = KType
  deriving stock (Eq, Show, Read, Data, Generic)

instance HasID Type where
  _id = _typeMetaLens % _id
