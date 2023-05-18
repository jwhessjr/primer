{-# LANGUAGE FunctionalDependencies #-}

-- | This module contains the zipper type @TypeZ@, and functions for
--  operating on them.
module Primer.Zipper.Type (
  focus,
  target,
  down,
  left,
  right,
  farthest,
  foldBelow,
  getBoundHereTy',
  getBoundHereTy,
  getBoundHereDnTy,
  bindersBelowTy,
) where

import Foreword

import Data.Data (Data)
import Data.Generics.Uniplate.Data ()
import Data.Generics.Uniplate.Zipper (
  Zipper,
  zipper,
 )
import Data.Generics.Uniplate.Zipper qualified as Z
import Data.Set qualified as S
import Optics (
  view,
 )
import Optics.Lens (Lens', equality')
import Optics.Traversal (traverseOf)
import Primer.Core.Meta (
  TyVarName,
 )
import Primer.Core.Type (
  Type',
 )


-- | We want to use up, down, left, right, etc. on 'ExprZ' and 'TypeZ',
-- despite them being very different types. This class enables that, by proxying
-- each method through to the underlying Zipper.
-- @za@ is the user-facing type, i.e. 'ExprZ' or 'TypeZ'.
-- @a@ is the type of targets of the internal zipper, i.e. 'Expr' or 'Type'.
class Data a => IsZipper za a | za -> a where
  asZipper :: Lens' za (Z.Zipper a a)

instance Data a => IsZipper (Z.Zipper a a) a where
  asZipper = equality'

target :: IsZipper za a => za -> a
target = Z.hole . view asZipper

down :: (IsZipper za a) => za -> Maybe za
down = traverseOf asZipper Z.down

left :: IsZipper za a => za -> Maybe za
left = traverseOf asZipper Z.left

right :: IsZipper za a => za -> Maybe za
right = traverseOf asZipper Z.right

-- | Convert a normal 'Expr' or 'Type' to a cursored one, focusing on the root
focus :: (Data a) => a -> Zipper a a
focus = zipper

-- | Move the zipper focus as far in one direction as possible
farthest :: (a -> Maybe a) -> a -> a
farthest f = go where go a = maybe a go (f a)

-- | Focus on the current thing, and then everything 'below', in depth-first,
-- leftmost-first order;
-- map each to a monoid, and accumulate
-- NB: 'foldAbove' + 'foldBelow' does not encompass the whole term: it misses
-- siblings.
foldBelow :: (IsZipper za a, Monoid m) => (a -> m) -> za -> m
foldBelow f z = f (target z) <> maybe mempty (go . farthest left) (down z)
  where
    go z' = f (target z') <> maybe mempty (go . farthest left) (down z') <> maybe mempty go (right z')

-- Note that we have two specialisations we care about:
-- bindersBelowTy :: TypeZip -> S.Set Name
-- bindersBelowTy :: Zipper (Type' One) (Type' One) -> S.Set Name
bindersBelowTy :: (Data a, Eq a) => Zipper (Type' a) (Type' a) -> S.Set TyVarName
bindersBelowTy = foldBelow getBoundHereDnTy

-- Get all names bound by this layer of an type, for any child.
getBoundHereDnTy :: Eq a => Type' a -> S.Set TyVarName
getBoundHereDnTy e = getBoundHereTy e Nothing

getBoundHereTy :: Eq a => Type' a -> Maybe (Type' a) -> S.Set TyVarName
getBoundHereTy t prev = S.fromList $ either identity (\(LetTypeBind n _) -> n) <$> getBoundHereTy' t prev

data LetTypeBinding' a = LetTypeBind TyVarName (Type' a)
  deriving stock (Eq, Show)

getBoundHereTy' :: Eq a => Type' a -> Maybe (Type' a) -> [Either TyVarName (LetTypeBinding' a)]
getBoundHereTy' t _prev = case t of
  _ -> mempty
