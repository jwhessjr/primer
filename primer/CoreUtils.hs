module CoreUtils (
  freshLocalName',
  exprIDs,
  typeIDs,
  regenerateTypeIDs,
  forgetTypeMetadata,
  regenerateExprIDs,
  noHoles,
  _freeTmVars,
  _freeTyVars,
  _freeVars,
  _freeVarsTy,
  freeGlobalVars,
  alphaEqTy,
) where

import Foreword

import Core (
  CaseBranch' (..),
  Expr' (..),
  GVarName,
  HasID (_id),
  ID,
  LVarName,
  LocalName (LocalName),
  TyVarName,
  Type' (..),
  bindName,
  _exprMeta,
  _exprTypeMeta,
  _typeMeta,
 )
import Data.Data (Data)
import Data.Generics.Uniplate.Data (universe)
import Data.Set qualified as S
import Fresh (MonadFresh, fresh)
import Name (Name, NameCounter, freshName)
import Optics (
  Fold,
  Traversal,
  Traversal',
  adjoin,
  getting,
  set,
  summing,
  to,
  traversalVL,
  traverseOf,
  (%),
 )

-- | Helper, wrapping 'freshName'
freshLocalName' :: MonadFresh NameCounter m => S.Set Name -> m (LocalName k)
freshLocalName' = fmap LocalName . freshName

-- | Regenerate all IDs, not changing any other metadata
regenerateTypeIDs :: (HasID a, MonadFresh ID m) => Type' a -> m (Type' a)
regenerateTypeIDs = regenerateTypeIDs' (set _id)

regenerateTypeIDs' :: MonadFresh ID m => (ID -> a -> b) -> Type' a -> m (Type' b)
regenerateTypeIDs' s = traverseOf _typeMeta (\a -> flip s a <$> fresh)

-- | Replace all 'ID's in a Type with unit.
-- Technically this replaces all annotations, regardless of what they are.
forgetTypeMetadata :: Type' a -> Type' ()
forgetTypeMetadata = set _typeMeta ()

-- | Test whether an type contains any holes
-- (empty or non-empty, or inside a kind)
noHoles :: Data a => Type' a -> Bool
noHoles t = flip all (universe t) $ \case
  TEmptyHole{} -> False
  _ -> True

_freeVarsTy :: Traversal (Type' a) (Type' a) (a, TyVarName) (Type' a)
_freeVarsTy = traversalVL $ traverseFreeVarsTy mempty

-- Helper for _freeVarsTy and _freeTyVars
-- Takes a set of considered-to-be-bound variables
traverseFreeVarsTy :: Applicative f => Set TyVarName -> ((a, TyVarName) -> f (Type' a)) -> Type' a -> f (Type' a)
traverseFreeVarsTy = go
  where
    go bound f = \case
      t@TEmptyHole{} -> pure t
      t@TCon{} -> pure t
      TFun m s t -> TFun m <$> go bound f s <*> go bound f t

-- Check two types for alpha equality
--
-- it makes usage easier if this is pure
-- i.e. we don't want to need a fresh name supply
-- We assume both inputs are both from the same context
--
-- Note that we do not expand TLets, they must be structurally
-- the same (perhaps with a different named binding)
alphaEqTy :: Type' () -> Type' () -> Bool
alphaEqTy = go
  where
    go (TEmptyHole _) (TEmptyHole _) = True
    go (TCon _ n) (TCon _ m) = n == m
    go (TFun _ a b) (TFun _ c d) = go a c && go b d
    go _ _ = False

-- | Traverse the 'ID's in a 'Type''.
typeIDs :: HasID a => Traversal' (Type' a) ID
typeIDs = _typeMeta % _id

-- | Regenerate all IDs, not changing any other metadata
regenerateExprIDs :: (HasID a, HasID b, MonadFresh ID m) => Expr' a b -> m (Expr' a b)
regenerateExprIDs = regenerateExprIDs' (set _id) (set _id)

regenerateExprIDs' :: MonadFresh ID m => (ID -> a -> a') -> (ID -> b -> b') -> Expr' a b -> m (Expr' a' b')
regenerateExprIDs' se st =
  traverseOf _exprMeta (\a -> flip se a <$> fresh)
    >=> traverseOf _exprTypeMeta (\a -> flip st a <$> fresh)

-- We can't offer a traversal, as we can't enforce replacing term vars with
-- terms and type vars with types. Use _freeTmVars and _freeTyVars for
-- traversals.
_freeVars :: Fold (Expr' a b) (Either (a, LVarName) (b, TyVarName))
_freeVars = getting _freeTmVars % to Left `summing` getting _freeTyVars % to Right

_freeTmVars :: Traversal (Expr' a b) (Expr' a b) (a, LVarName) (Expr' a b)
_freeTmVars = traversalVL $ go mempty
  where
    go :: Applicative f => Set LVarName -> ((a, LVarName) -> f (Expr' a b)) -> Expr' a b -> f (Expr' a b)
    go bound f = \case
      Hole m e -> Hole m <$> go bound f e
      t@EmptyHole{} -> pure t
      Ann m e ty -> Ann m <$> go bound f e <*> pure ty
      Case m e bs -> Case m <$> go bound f e <*> traverse freeVarsBr bs
      where
        freeVarsBr (CaseBranch c binds e) = CaseBranch c binds <$> go (S.union bound $ S.fromList $ map bindName binds) f e

_freeTyVars :: Traversal (Expr' a b) (Expr' a b) (b, TyVarName) (Type' b)
_freeTyVars = traversalVL $ go mempty
  where
    go :: Applicative f => Set TyVarName -> ((b, TyVarName) -> f (Type' b)) -> Expr' a b -> f (Expr' a b)
    go bound f = \case
      Hole m e -> Hole m <$> go bound f e
      t@EmptyHole{} -> pure t
      Ann m e ty -> Ann m <$> go bound f e <*> traverseFreeVarsTy bound f ty
      Case m e bs -> Case m <$> go bound f e <*> traverse freeVarsBr bs
      where
        freeVarsBr (CaseBranch c binds e) = CaseBranch c binds <$> go bound f e -- case branches only bind term variables

freeGlobalVars :: Expr' a b -> Set GVarName
freeGlobalVars _ = mempty

-- | Traverse the 'ID's in an 'Expr''.
exprIDs :: (HasID a, HasID b) => Traversal' (Expr' a b) ID
exprIDs = (_exprMeta % _id) `adjoin` (_exprTypeMeta % _id)
