{-# LANGUAGE FunctionalDependencies #-}

-- | This module contains the zipper types @ExprZ@ and @TypeZ@, and functions for
--  operating on them.
module Primer.Zipper (
  ExprZ,
  TypeZip,
  TypeZ,
  CaseBindZ,
  updateCaseBind,
  unfocusCaseBind,
  caseBindZFocus,
  IsZipper (asZipper),
  Loc,
  Loc' (..),
  BindLoc,
  BindLoc' (..),
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
  FoldAbove (FA, current, prior),
  foldAbove,
  foldBelow,
  unfocusExpr,
  unfocusLoc,
  locToEither,
  bindersAbove,
  bindersBelow,
  getBoundHere,
  getBoundHereUp,
  bindersAboveTy,
  bindersAboveTypeZ,
  getBoundHereTy,
  bindersBelowTy,
) where

import Foreword

import Data.Data (Data)
import Data.Generics.Product (field, position)
import Data.Generics.Sum (_Ctor)
import Data.Generics.Uniplate.Data ()
import Data.Generics.Uniplate.Zipper (
  Zipper,
  fromZipper,
  replaceHole,
  zipper,
 )
import qualified Data.Generics.Uniplate.Zipper as Z
import Data.List as List (delete)
import qualified Data.Set as S
import Optics (
  filteredBy,
  ifolded,
  iheadOf,
  iso,
  ix,
  only,
  over,
  preview,
  set,
  view,
  (%),
  (.~),
  (<%),
  (<%>),
  (^?),
 )
import Optics.Lens (Lens', equality', lens)
import Optics.Traversal (traverseOf)
import Primer.Core (
  Bind' (..),
  CaseBranch' (CaseBranch),
  Expr,
  Expr' (Case, LAM, Lam, Let, LetType, Letrec),
  ExprMeta,
  HasID (..),
  ID,
  LocalName (unLocalName),
  TyVarName,
  Type' (TForall),
  TypeMeta,
  bindName,
  getID,
  typesInExpr,
 )
import Primer.Name (Name)

type ExprZ' a b = Zipper (Expr' a b) (Expr' a b)

-- | An ordinary zipper for 'Expr's
type ExprZ = ExprZ' ExprMeta TypeMeta

type TypeZip' b = Zipper (Type' b) (Type' b)

-- | An ordinary zipper for 'Type's
type TypeZip = TypeZip' TypeMeta

-- | A zipper for 'Type's embedded in expressions.
-- For such types, we need a way
-- to navigate around them without losing our place in the wider expression.
-- This type contains a Zipper for a 'Type' and a function that will place the
-- unzippered type back into the wider expression zipper, keeping its place.
data TypeZ' a b = TypeZ (TypeZip' b) (Type' b -> ExprZ' a b)
  deriving (Generic)

type TypeZ = TypeZ' ExprMeta TypeMeta

instance HasID b => HasID (TypeZ' a b) where
  _id = position @1 % _id

-- | A zipper for variable bindings in case branches.
-- This type focuses on a particular binding in a particular branch.
-- It contains the focused binding, along with the the parent expression (as a zipper) and some
-- parts of the surrounding branch which are useful when renaming.
-- It also contains a function which can update the binding and the RHS of the case branch
-- simultaneously, yielding a new expression.
-- These fields are chosen to be convenient for renaming, and they may not be that useful for future
-- actions we want to perform.
data CaseBindZ' a b = CaseBindZ
  { caseBindZExpr :: ExprZ' a b
  -- ^ a zipper focused on the case expression
  , caseBindZFocus :: Bind' a
  -- ^ the focused binding
  , caseBindZRhs :: Expr' a b
  -- ^ the rhs of the branch
  , caseBindAllBindings :: [Bind' a]
  -- ^ all other bindings in the case branch, i.e. all except the focused one
  , caseBindZUpdate :: Bind' a -> Expr' a b -> ExprZ' a b -> ExprZ' a b
  -- ^ a function to update the focused binding and rhs simultaneously
  }
  deriving (Generic)

type CaseBindZ = CaseBindZ' ExprMeta TypeMeta

-- Apply an update function to the focus of a case binding, optionally modifying the rhs of the branch too.
-- The update function is given three arguments:
-- - the focused binding
-- - a list of all other bindings in the branch (not including the focused one)
-- - the rhs of the branch
-- It returns a tuple of the updated binding and the updated rhs.
-- This is very specialised to be useful when renaming case branch bindings.
-- It may not be very reusable but I think it's helpful to keep the complexity of 'CaseBindZ'
-- restricted to this module.
updateCaseBind ::
  Functor f =>
  CaseBindZ ->
  (Bind' ExprMeta -> [Bind' ExprMeta] -> Expr -> f (Bind' ExprMeta, Expr)) ->
  f CaseBindZ
updateCaseBind (CaseBindZ z bind rhs bindings update) f =
  f bind bindings rhs <&> \(bind', rhs') ->
    let z' = update bind' rhs' z
     in CaseBindZ z' bind' rhs' bindings update

instance HasID a => HasID (CaseBindZ' a b) where
  _id = field @"caseBindZFocus" % _id

-- | A specific location in our AST.
-- This can either be in an expression, type, or binding.
data Loc' a b
  = -- | An expression
    InExpr (ExprZ' a b)
  | -- | A type
    InType (TypeZ' a b)
  | -- | A binding (currently just case bindings)
    InBind (BindLoc' a b)
  deriving (Generic)

type Loc = Loc' ExprMeta TypeMeta

instance (HasID a, HasID b) => HasID (Loc' a b) where
  _id = lens getter setter
    where
      getter = \case
        InExpr e -> view _id e
        InType l -> view _id l
        InBind l -> view _id l
      setter l i = case l of
        InExpr e -> InExpr $ set _id i e
        InType t -> InType $ set _id i t
        InBind t -> InBind $ set _id i t

-- | A location of a binding.
-- This only covers bindings in case branches for now.

{- HLINT ignore BindLoc' "Use newtype instead of data" -}
data BindLoc' a b
  = BindCase (CaseBindZ' a b)
  deriving (Generic)

type BindLoc = BindLoc' ExprMeta TypeMeta

instance HasID a => HasID (BindLoc' a b) where
  _id = position @1 % _id

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

-- | If the currently focused expression is a case expression, search the bindings of its branches
-- to find one matching the given ID, and return the 'Loc' for that binding.
-- If no match is found, return @Nothing@.
findInCaseBinds :: (Data a, Data b, Eq a, HasID a) => ID -> ExprZ' a b -> Maybe (Loc' a b)
findInCaseBinds i z = do
  let branchesLens = _target % _Ctor @"Case" % position @3
  branches <- preview branchesLens z
  ((branchIx, bindIx), bind) <- branches & iheadOf (ifolded % position @2 <%> ifolded <% filteredBy (_Ctor @"Bind" % position @1 % _id % only i))
  let branchLens = branchesLens % ix branchIx
  let rhsLens = branchLens % position @3
  rhs <- preview rhsLens z
  allBinds <- preview (branchLens % position @2) z
  let bindLens = branchLens % position @2 % ix bindIx
  let update bind' rhs' = set rhsLens rhs' . set bindLens bind'
  pure $ InBind $ BindCase $ CaseBindZ z bind rhs (delete bind allBinds) update

-- | Switch from a 'Type' zipper back to an 'Expr' zipper.
unfocusType :: TypeZ' a b -> ExprZ' a b
unfocusType (TypeZ zt f) = f (fromZipper zt)

-- | Forget the surrounding expression context
focusOnlyType :: TypeZ' a b -> TypeZip' b
focusOnlyType (TypeZ zt _) = zt

-- | We want to use up, down, left, right, etc. on 'ExprZ' and 'TypeZ',
-- despite them being very different types. This class enables that, by proxying
-- each method through to the underlying Zipper.
-- @za@ is the user-facing type, i.e. 'ExprZ' or 'TypeZ'.
-- @a@ is the type of targets of the internal zipper, i.e. 'Expr' or 'Type'.
class Data a => IsZipper za a | za -> a where
  asZipper :: Lens' za (Z.Zipper a a)

instance Data a => IsZipper (Z.Zipper a a) a where
  asZipper = equality'

instance Data b => IsZipper (TypeZ' a b) (Type' b) where
  asZipper = position @1

-- 'CaseBindZ' is sort of a fake zipper which can only focus on one thing: the case binding.
-- It's a bit fiddly to make it appear as a zipper like this, but it's convenient to have a
-- consistent interface for 'ExprZ', 'TypeZ' and 'CaseBindZ'.
instance IsZipper CaseBindZ (Bind' ExprMeta) where
  asZipper = field @"caseBindZFocus" % iso zipper fromZipper

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

-- | Convert a normal 'Expr' to a cursored one, focusing on the root
focus :: (Data a) => a -> Zipper a a
focus = zipper

-- | Convert an 'Expr' to a 'Loc' which focuses on the top of the expression.
focusLoc :: Expr -> Loc
focusLoc = InExpr . focus

-- Convert a 'CaseBindZ' to an 'ExprZ' by shifting focus to the parent case expression.
unfocusCaseBind :: CaseBindZ' a b -> ExprZ' a b
unfocusCaseBind = caseBindZExpr

-- | Convert an 'Expr' zipper to an 'Expr'
unfocusExpr :: ExprZ' a b -> Expr' a b
unfocusExpr = fromZipper

-- | Convert a 'Loc' to an 'ExprZ'.
-- If we're in a type or case binding, we'll shift focus up to the nearest enclosing expression.
unfocusLoc :: Loc -> ExprZ
unfocusLoc (InExpr z) = z
unfocusLoc (InType z) = unfocusType z
unfocusLoc (InBind (BindCase z)) = unfocusCaseBind z

-- | Convert a 'Loc' to 'Either ExprZ TypeZ'.
-- If the 'Loc' is in a case bind, we shift focus to the parent case expression.
-- This function is mainly to keep compatibility with code which still expects 'Either ExprZ TypeZ'
-- as a representation of an AST location.
locToEither :: Loc' a b -> Either (ExprZ' a b) (TypeZ' a b)
locToEither (InBind (BindCase z)) = Left $ unfocusCaseBind z
locToEither (InExpr z) = Left z
locToEither (InType z) = Right z

-- | Convert a 'Loc' to an 'Expr'.
-- This shifts focus right up to the top, so the result is the whole expression.
unfocus :: Loc -> Expr
unfocus = unfocusExpr . unfocusLoc

-- | Replace the node at the cursor with the given value.
replace :: (IsZipper za a) => a -> za -> za
replace = over asZipper . replaceHole

-- | Focus on the node with the given 'ID', if it exists in the expression
focusOn :: (Data a, Data b, Eq a, HasID a, HasID b) => ID -> Expr' a b -> Maybe (Loc' a b)
focusOn i = focusOn' i . focus

-- | Focus on the node with the given 'ID', if it exists in the focussed expression
focusOn' :: (Data a, Data b, Eq a, HasID a, HasID b) => ID -> ExprZ' a b -> Maybe (Loc' a b)
focusOn' i = fmap snd . search matchesID
  where
    matchesID z
      -- If the current target has the correct ID, return that
      | getID (target z) == i = Just $ InExpr z
      -- If the target has an embedded type, search the type for a match.
      -- If the target is a case expression with bindings, search each binding for a match.
      | otherwise =
          let inType = focusType z >>= search (guarded (== i) . getID . target) <&> fst <&> InType
              inCaseBinds = findInCaseBinds i z
           in inType <|> inCaseBinds

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

data FoldAbove a = FA {prior :: a, current :: a}

-- | Focus on everything 'up', in order, map each to a monoid, and accumulate.
-- This does not focus on the current target.
-- We keep track of which child we just came up from, as that can be important
-- in applications: e.g. finding enclosing binders, where @let x=e1 in e2@ has
-- two children, but only binds a variable in one of them.
-- NB: 'foldAbove' + 'foldBelow' does not encompass the whole term: it misses
-- siblings.
foldAbove :: (IsZipper za a, Monoid m) => (FoldAbove a -> m) -> za -> m
foldAbove f z = go (target z) (up z)
  where
    go p c = case c of
      Nothing -> mempty
      Just z' -> let cur = target z' in f (FA{prior = p, current = cur}) <> go cur (up z')

-- | Focus on the current thing, and then everything 'below', in depth-first,
-- leftmost-first order;
-- map each to a monoid, and accumulate
-- NB: 'foldAbove' + 'foldBelow' does not encompass the whole term: it misses
-- siblings.
foldBelow :: (IsZipper za a, Monoid m) => (a -> m) -> za -> m
foldBelow f z = f (target z) <> maybe mempty (go . farthest left) (down z)
  where
    go z' = f (target z') <> maybe mempty (go . farthest left) (down z') <> maybe mempty go (right z')

-- Gets all binders that scope over the focussed subtree
bindersAbove :: ExprZ -> S.Set Name
bindersAbove = foldAbove getBoundHereUp

bindersAboveTy :: TypeZip -> S.Set TyVarName
bindersAboveTy = foldAbove (getBoundHereTy . current)

-- Note that we have two specialisations we care about:
-- bindersBelowTy :: TypeZip -> S.Set Name
-- bindersBelowTy :: Zipper (Type' One) (Type' One) -> S.Set Name
bindersBelowTy :: Data a => Zipper (Type' a) (Type' a) -> S.Set TyVarName
bindersBelowTy = foldBelow getBoundHereTy

bindersAboveTypeZ :: TypeZ -> S.Set Name
bindersAboveTypeZ t =
  let moreGlobal = S.map unLocalName $ bindersAboveTy $ focusOnlyType t
      e = unfocusType t
      -- Since nothing both contains a type and binds a variable, we
      -- know moreGlobalHere will be empty, but let's keep it around as future
      -- proofing
      moreGlobalHere = getBoundHere (target e) Nothing
      moreGlobal' = bindersAbove e <> moreGlobalHere
   in moreGlobal <> moreGlobal'

-- Get the names bound by this layer of an expression for a given child.
getBoundHereUp :: (Eq a, Eq b) => FoldAbove (Expr' a b) -> S.Set Name
getBoundHereUp e = getBoundHere (current e) (Just $ prior e)

-- Get the names bound within the focussed subtree
bindersBelow :: ExprZ -> S.Set Name
bindersBelow = foldBelow getBoundHereDn

-- Get all names bound by this layer of an expression, for any child.
-- E.g. for a "match" we get all vars bound by each branch.
getBoundHereDn :: (Eq a, Eq b) => Expr' a b -> S.Set Name
getBoundHereDn e = getBoundHere e Nothing

-- Get the names bound by this layer of an expression (both term and type names)
-- The second arg is the child we just came out of, if traversing up (and thus
-- need to extract binders based on which case branch etc), and Nothing if
-- traversing down (and want to get all binders regardless of branch).
getBoundHere :: (Eq a, Eq b) => Expr' a b -> Maybe (Expr' a b) -> S.Set Name
getBoundHere e prev = case e of
  Lam _ v _ -> singleton v
  LAM _ tv _ -> singleton tv
  Let _ v _ b ->
    if maybe True (== b) prev
      then singleton v
      else mempty
  Letrec _ v _ _ _ -> singleton v
  LetType _ v _ _ -> singleton v
  Case _ _ bs ->
    let binderss = map (\(CaseBranch _ ns rhs) -> (rhs, S.fromList $ map (unLocalName . bindName) ns)) bs
     in case prev of
          Nothing -> S.unions $ map snd binderss
          Just p -> S.unions $ map (\(b, binders) -> if b == p then binders else mempty) binderss
  _ -> mempty
  where
    singleton = S.singleton . unLocalName

getBoundHereTy :: Type' a -> S.Set TyVarName
getBoundHereTy = \case
  TForall _ v _ _ -> S.singleton v
  _ -> mempty
