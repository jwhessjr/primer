{-# LANGUAGE FunctionalDependencies #-}

-- | This module contains the zipper types @ExprZ@ and @TypeZ@, and functions for
--  operating on them.
module Zipper (
  ExprZ,
  TypeZip,
  TypeZ,
  IsZipper (asZipper),
  Loc,
  Loc' (..),
  focusType,
  focusLoc,
  unfocusType,
  focusOnlyType,
  focus,
  unfocus,
  target,
  _target,
  replace,
  focusOn,
  focusOnTy,
  top,
  up,
  down,
  left,
  right,
  farthest,
  unfocusExpr,
  unfocusLoc,
  locToEither,
  SomeNode (..),
  findNodeWithParent,
  findType,
) where

import Foreword

import Core (
  Expr,
  Expr',
  ExprMeta,
  HasID (..),
  ID,
  Type,
  Type' (),
  TypeMeta,
  getID,
  typesInExpr,
 )
import Data.Data (Data)
import Data.Generics.Uniplate.Data ()
import Data.Generics.Uniplate.Zipper (
  Zipper,
  fromZipper,
  replaceHole,
  zipper,
 )
import Data.Generics.Uniplate.Zipper qualified as Z
import Optics (
  over,
  set,
  view,
  (%),
  (.~),
  (^?),
 )
import Optics.Lens (Lens', equality', lens)
import Optics.Traversal (traverseOf)

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

type ExprZ' a b = Zipper (Expr' a b) (Expr' a b)

-- | An ordinary zipper for 'Expr's
type ExprZ = ExprZ' ExprMeta TypeMeta

-- | A zipper for 'Type's embedded in expressions.
-- For such types, we need a way
-- to navigate around them without losing our place in the wider expression.
-- This type contains a Zipper for a 'Type' and a function that will place the
-- unzippered type back into the wider expression zipper, keeping its place.
data TypeZ' a b = TypeZ (TypeZip' b) (Type' b -> ExprZ' a b)

tzpos1 :: Lens' (TypeZ' a b) (TypeZip' b)
tzpos1 = lens (\(TypeZ z _) -> z) (\(TypeZ _ f) z -> TypeZ z f)

type TypeZ = TypeZ' ExprMeta TypeMeta

instance HasID b => HasID (TypeZ' a b) where
  _id = tzpos1 % _id

-- | A specific location in our AST.
-- This can either be in an expression, type, or binding.
data Loc' a b
  = -- | An expression
    InExpr (ExprZ' a b)
  | -- | A type
    InType (TypeZ' a b)

type Loc = Loc' ExprMeta TypeMeta

instance (HasID a, HasID b) => HasID (Loc' a b) where
  _id = lens getter setter
    where
      getter = \case
        InExpr e -> view _id e
        InType l -> view _id l
      setter l i = case l of
        InExpr e -> InExpr $ set _id i e
        InType t -> InType $ set _id i t

-- | A location of a binding.
-- This only covers bindings in case branches for now.

-- | Switch from an 'Expr' zipper to a 'Type' zipper, focusing on the type in
-- the current target. This expects that the target is an @Ann@, @App@,
-- @Letrec@ or @LetType@ node (as those are the only ones that contain a
-- @Type@).
focusType :: (Data a, Data b) => ExprZ' a b -> Maybe (TypeZ' a b)
focusType z = do
  t <- z ^? l
  pure $ TypeZ (zipper t) $ \t' -> z & l .~ t'
  where
    l = _target % typesInExpr

-- | Switch from a 'Type' zipper back to an 'Expr' zipper.
unfocusType :: TypeZ' a b -> ExprZ' a b
unfocusType (TypeZ zt f) = f (fromZipper zt)

-- | Forget the surrounding expression context
focusOnlyType :: TypeZ' a b -> TypeZip' b
focusOnlyType (TypeZ zt _) = zt

instance Data b => IsZipper (TypeZ' a b) (Type' b) where
  asZipper = tzpos1

-- | Convert an 'Expr' to a 'Loc' which focuses on the top of the expression.
focusLoc :: Expr -> Loc
focusLoc = InExpr . focus

-- | Convert an 'Expr' zipper to an 'Expr'
unfocusExpr :: ExprZ' a b -> Expr' a b
unfocusExpr = fromZipper

-- | Convert a 'Loc' to an 'ExprZ'.
-- If we're in a type or case binding, we'll shift focus up to the nearest enclosing expression.
unfocusLoc :: Loc -> ExprZ
unfocusLoc (InExpr z) = z
unfocusLoc (InType z) = unfocusType z

-- | Convert a 'Loc' to 'Either ExprZ TypeZ'.
-- If the 'Loc' is in a case bind, we shift focus to the parent case expression.
-- This function is mainly to keep compatibility with code which still expects 'Either ExprZ TypeZ'
-- as a representation of an AST location.
locToEither :: Loc' a b -> Either (ExprZ' a b) (TypeZ' a b)
locToEither (InExpr z) = Left z
locToEither (InType z) = Right z

-- | Convert a 'Loc' to an 'Expr'.
-- This shifts focus right up to the top, so the result is the whole expression.
unfocus :: Loc -> Expr
unfocus = unfocusExpr . unfocusLoc

-- | Focus on the node with the given 'ID', if it exists in the expression
focusOn :: (Data a, Data b, HasID a, HasID b) => ID -> Expr' a b -> Maybe (Loc' a b)
focusOn i = focusOn' i . focus

-- | Focus on the node with the given 'ID', if it exists in the focussed expression
focusOn' :: (Data a, Data b, HasID a, HasID b) => ID -> ExprZ' a b -> Maybe (Loc' a b)
focusOn' i = fmap snd . search matchesID
  where
    matchesID z
      -- If the current target has the correct ID, return that
      | getID (target z) == i = Just $ InExpr z
      -- If the target has an embedded type, search the type for a match.
      -- If the target is a case expression with bindings, search each binding for a match.
      | otherwise =
          let inType = focusType z >>= search (guarded (== i) . getID . target) <&> fst <&> InType
           in inType

-- | Find a node in the AST by its ID, and also return its parent
findNodeWithParent ::
  ID ->
  Expr ->
  Maybe (SomeNode, Maybe SomeNode)
findNodeWithParent id x = do
  z <- focusOn id x
  Just $ case z of
    InExpr ez -> (ExprNode $ target ez, ExprNode . target <$> up ez)
    InType tz ->
      ( TypeNode $ target tz
      , Just $
          maybe
            (ExprNode $ target $ unfocusType tz)
            (TypeNode . target)
            (up tz)
      )

-- | Find a sub-type in a larger type by its ID.
findType :: ID -> Type -> Maybe Type
findType id ty = target <$> focusOnTy id ty

-- | An AST node tagged with its "sort" - i.e. if it's a type or expression or binding etc.
data SomeNode
  = ExprNode Expr
  | TypeNode Type
  deriving stock (Eq, Show, Read)
