{-# LANGUAGE FunctionalDependencies #-}

-- | This module contains the zipper type @TypeZ@, and functions for
--  operating on them.
module Primer.Zipper.Type (
  TypeZip,
  IsZipper (asZipper),
  focus,
  target,
  _target,
  replace,
  focusOnTy,
  top,
  up,
  down,
  left,
  right,
  farthest,
  search,
  FoldAbove,
  FoldAbove' (FA, current, prior),
  LetTypeBinding' (LetTypeBind),
  LetTypeBinding,
) where

import Foreword

import Data.Data (Data)
import Data.Generics.Uniplate.Data ()
import Data.Generics.Uniplate.Zipper (
  Zipper,
  replaceHole,
  zipper,
 )
import Data.Generics.Uniplate.Zipper qualified as Z
import Optics (
  over,
  view,
 )
import Optics.Lens (Lens', equality', lens)
import Optics.Traversal (traverseOf)
import Primer.Core.Meta (
  HasID (..),
  ID,
  TyVarName,
  getID,
 )
import Primer.Core.Type (
  Type',
  TypeMeta,
 )

type TypeZip' b = Zipper (Type' b) (Type' b)

-- | An ordinary zipper for 'Type's
type TypeZip = TypeZip' TypeMeta

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

-- | A 'Lens' for the target of a zipper
_target :: IsZipper za a => Lens' za a
_target = lens target (flip replace)

up :: IsZipper za a => za -> Maybe za
up = traverseOf asZipper Z.up

down :: (IsZipper za a) => za -> Maybe za
down = traverseOf asZipper Z.down

left :: IsZipper za a => za -> Maybe za
left = traverseOf asZipper Z.left

right :: IsZipper za a => za -> Maybe za
right = traverseOf asZipper Z.right

top :: IsZipper za a => za -> za
top = farthest up

-- | Convert a normal 'Expr' or 'Type' to a cursored one, focusing on the root
focus :: (Data a) => a -> Zipper a a
focus = zipper

-- | Replace the node at the cursor with the given value.
replace :: (IsZipper za a) => a -> za -> za
replace = over asZipper . replaceHole

-- | Focus on the node with the given 'ID', if it exists in the type
focusOnTy ::
  (Data b, HasID b) =>
  ID ->
  Type' b ->
  Maybe (Zipper (Type' b) (Type' b))
focusOnTy i = focusOnTy' i . focus

-- | Focus on the node with the given 'ID', if it exists in the focussed type
focusOnTy' ::
  (Data b, HasID b) =>
  ID ->
  Zipper (Type' b) (Type' b) ->
  Maybe (Zipper (Type' b) (Type' b))
focusOnTy' i = fmap snd . search matchesID
  where
    matchesID z
      -- If the current target has the correct ID, return that
      | getID (target z) == i = Just z
      | otherwise = Nothing

-- | Search for a node for which @f@ returns @Just@ something.
-- Performs a depth-first, leftmost-first search.
search :: (IsZipper za a) => (za -> Maybe b) -> za -> Maybe (za, b)
search f z
  | Just x <- f z = Just (z, x)
  | otherwise =
      -- if the node has children, recurse on the leftmost child
      (down z >>= search f . farthest left)
        -- then recurse on the sibling to the right
        <|> (right z >>= search f)

-- | Move the zipper focus as far in one direction as possible
farthest :: (a -> Maybe a) -> a -> a
farthest f = go where go a = maybe a go (f a)

data FoldAbove' a b = FA {prior :: a, current :: b}
type FoldAbove a = FoldAbove' a a

data LetTypeBinding' a = LetTypeBind TyVarName (Type' a)
  deriving stock (Eq, Show)
type LetTypeBinding = LetTypeBinding' TypeMeta
