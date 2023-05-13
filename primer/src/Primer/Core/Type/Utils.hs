module Primer.Core.Type.Utils (
  typeIDs,
  generateTypeIDs,
  regenerateTypeIDs,
  forgetTypeMetadata,
  noHoles,
  _freeVarsTy,
  traverseFreeVarsTy,
  freeVarsTy,
  alphaEqTy,
) where

import Foreword

import Control.Monad.Fresh (MonadFresh, fresh)
import Data.Data (Data)
import Data.Generics.Uniplate.Data (universe)
import Data.Set qualified as S
import Data.Set.Optics (setOf)
import Optics (
  Traversal,
  Traversal',
  getting,
  set,
  traversalVL,
  traverseOf,
  (%),
  _2,
 )

import Primer.Core.Meta (
  HasID (_id),
  ID,
  TyVarName,
  trivialMeta,
 )
import Primer.Core.Type (
  Type,
  Type' (..),
  _typeMeta,
 )

-- | Regenerate all IDs, not changing any other metadata
regenerateTypeIDs :: (HasID a, MonadFresh ID m) => Type' a -> m (Type' a)
regenerateTypeIDs = regenerateTypeIDs' (set _id)

regenerateTypeIDs' :: MonadFresh ID m => (ID -> a -> b) -> Type' a -> m (Type' b)
regenerateTypeIDs' s = traverseOf _typeMeta (\a -> flip s a <$> fresh)

-- | Adds 'ID's and trivial metadata
generateTypeIDs :: MonadFresh ID m => Type' () -> m Type
generateTypeIDs = regenerateTypeIDs' $ const . trivialMeta

-- | Replace all 'ID's in a Type with unit.
-- Technically this replaces all annotations, regardless of what they are.
forgetTypeMetadata :: Type' a -> Type' ()
forgetTypeMetadata = set _typeMeta ()

-- | Test whether an type contains any holes
-- (empty or non-empty, or inside a kind)
noHoles :: Data a => Type' a -> Bool
noHoles t = flip all (universe t) $ \case
  THole{} -> False
  TEmptyHole{} -> False
  _ -> True

freeVarsTy :: Type' a -> Set TyVarName
freeVarsTy = setOf (getting _freeVarsTy % _2)

_freeVarsTy :: Traversal (Type' a) (Type' a) (a, TyVarName) (Type' a)
_freeVarsTy = traversalVL $ traverseFreeVarsTy mempty

-- Helper for _freeVarsTy and _freeTyVars
-- Takes a set of considered-to-be-bound variables
traverseFreeVarsTy :: Applicative f => Set TyVarName -> ((a, TyVarName) -> f (Type' a)) -> Type' a -> f (Type' a)
traverseFreeVarsTy = go
  where
    go bound f = \case
      t@TEmptyHole{} -> pure t
      THole m t -> THole m <$> go bound f t
      t@TCon{} -> pure t
      TFun m s t -> TFun m <$> go bound f s <*> go bound f t
      v@(TVar m a)
        | S.member a bound -> pure v
        | otherwise -> curry f m a
      TApp m s t -> TApp m <$> go bound f s <*> go bound f t

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
    go (THole _ s) (THole _ t) = go s t
    go (TCon _ n) (TCon _ m) = n == m
    go  (TFun _ a b) (TFun _ c d) = go a c && go b d
    go (TVar _ n) (TVar _ m) = n == m
    go (TApp _ a b) (TApp _ c d) = go a c && go b d
    go _ _ = False

-- | Traverse the 'ID's in a 'Type''.
typeIDs :: HasID a => Traversal' (Type' a) ID
typeIDs = _typeMeta % _id
