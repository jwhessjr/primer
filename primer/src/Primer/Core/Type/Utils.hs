module Primer.Core.Type.Utils (
  forgetTypeMetadata,
  _freeVarsTy,
  traverseFreeVarsTy,
  freeVarsTy,
  alphaEqTy,
) where

import Foreword

import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Data.Set.Optics (setOf)
import Optics (
  Traversal,
  getting,
  set,
  traversalVL,
  (%),
  _2,
 )

import Primer.Core.Meta (
  TyVarName,
 )
import Primer.Core.Type (
  Type' (..),
  _typeMeta,
 )

-- | Replace all 'ID's in a Type with unit.
-- Technically this replaces all annotations, regardless of what they are.
forgetTypeMetadata :: Type' a -> Type' ()
forgetTypeMetadata = set _typeMeta ()

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
      TForall m a k s -> TForall m a k <$> go (S.insert a bound) f s
      TLet m a t b -> TLet m a <$> go bound f t <*> go (S.insert a bound) f b

-- Check two types for alpha equality
--
-- it makes usage easier if this is pure
-- i.e. we don't want to need a fresh name supply
-- We assume both inputs are both from the same context
--
-- Note that we do not expand TLets, they must be structurally
-- the same (perhaps with a different named binding)
alphaEqTy :: Type' () -> Type' () -> Bool
alphaEqTy = go (0, mempty, mempty)
  where
    go _ (TEmptyHole _) (TEmptyHole _) = True
    go bs (THole _ s) (THole _ t) = go bs s t
    go _ (TCon _ n) (TCon _ m) = n == m
    go bs (TFun _ a b) (TFun _ c d) = go bs a c && go bs b d
    go (_, p, q) (TVar _ n) (TVar _ m) = p ! n == q ! m
    go bs (TApp _ a b) (TApp _ c d) = go bs a c && go bs b d
    go bs (TForall _ n k s) (TForall _ m l t) = k == l && go (new bs n m) s t
    go bs (TLet _ a t b) (TLet _ c s d) = go bs t s && go (new bs a c) b d
    go _ _ _ = False
    p ! n = case p M.!? n of
      Nothing -> Left n -- free vars: compare by name
      Just i -> Right i -- bound vars: up to alpha
      -- Note that the maps 'p' and 'q' map names to "which forall
      -- they came from", in some sense.  The @c@ value is how many
      -- binders we have gone under, and is thus the next value free
      -- in the map.
    new (c, p, q) n m = (c + 1 :: Int, M.insert n c p, M.insert m c q)
