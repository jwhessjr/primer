module CoreUtils (
  exprIDs,
  typeIDs,
  regenerateTypeIDs,
  forgetTypeMetadata,
  regenerateExprIDs,
  noHoles,
  freeGlobalVars,
  alphaEqTy,
) where

import Foreword

import Core (
  Expr' (..),
  GVarName,
  HasID (_id),
  ID,
  Type' (..),
  _exprMeta,
  _exprTypeMeta,
  _typeMeta,
 )
import Data.Data (Data)
import Data.Generics.Uniplate.Data (universe)
import Fresh (MonadFresh, fresh)
import Optics (
  Traversal',
  adjoin,
  set,
  traverseOf,
  (%),
 )

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

freeGlobalVars :: Expr' a b -> Set GVarName
freeGlobalVars _ = mempty

-- | Traverse the 'ID's in an 'Expr''.
exprIDs :: (HasID a, HasID b) => Traversal' (Expr' a b) ID
exprIDs = (_exprMeta % _id) `adjoin` (_exprTypeMeta % _id)
