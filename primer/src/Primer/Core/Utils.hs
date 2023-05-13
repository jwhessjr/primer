module Primer.Core.Utils (
  freshLocalName,
  freshLocalName',
  exprIDs,
  typeIDs,
  generateTypeIDs,
  regenerateTypeIDs,
  forgetTypeMetadata,
  regenerateExprIDs,
  noHoles,
  _freeTmVars,
  _freeTyVars,
  _freeVars,
  _freeVarsTy,
  freeVarsTy,
  freeGlobalVars,
  alphaEqTy,
) where

import Foreword

import Control.Monad.Fresh (MonadFresh, fresh)
import Data.Data (Data)
import Data.Generics.Uniplate.Data (universe)
import Data.Set qualified as S
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

import Primer.Core (
  CaseBranch' (..),
  Expr' (..),
  GVarName,
  HasID (_id),
  ID,
  LVarName,
  TmVarRef (GlobalVarRef, LocalVarRef),
  TyVarName,
  Type' (..),
  bindName,
  _exprMeta,
  _exprTypeMeta,
 )
import Primer.Core.Fresh (freshLocalName, freshLocalName')
import Primer.Core.Type.Utils (
  alphaEqTy,
  forgetTypeMetadata,
  freeVarsTy,
  generateTypeIDs,
  noHoles,
  regenerateTypeIDs,
  traverseFreeVarsTy,
  typeIDs,
  _freeVarsTy,
 )

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
      t@EmptyHole{} -> pure t
      Ann m e ty -> Ann m <$> go bound f e <*> pure ty
      Lam m v e -> Lam m v <$> go (S.insert v bound) f e
      Case m e bs -> Case m <$> go bound f e <*> traverse freeVarsBr bs
      where
        freeVarsBr (CaseBranch c binds e) = CaseBranch c binds <$> go (S.union bound $ S.fromList $ map bindName binds) f e

_freeTyVars :: Traversal (Expr' a b) (Expr' a b) (b, TyVarName) (Type' b)
_freeTyVars = traversalVL $ go mempty
  where
    go :: Applicative f => Set TyVarName -> ((b, TyVarName) -> f (Type' b)) -> Expr' a b -> f (Expr' a b)
    go bound f = \case
      t@EmptyHole{} -> pure t
      Ann m e ty -> Ann m <$> go bound f e <*> traverseFreeVarsTy bound f ty
      Lam m v e ->
        -- A well scoped term will not refer to v as a type
        -- variable, so we do not need to add it to the bound set
        Lam m v <$> go bound f e
      Case m e bs -> Case m <$> go bound f e <*> traverse freeVarsBr bs
      where
        freeVarsBr (CaseBranch c binds e) = CaseBranch c binds <$> go bound f e -- case branches only bind term variables

freeGlobalVars :: (Data a, Data b) => Expr' a b -> Set GVarName
freeGlobalVars e = mempty

-- | Traverse the 'ID's in an 'Expr''.
exprIDs :: (HasID a, HasID b) => Traversal' (Expr' a b) ID
exprIDs = (_exprMeta % _id) `adjoin` (_exprTypeMeta % _id)
