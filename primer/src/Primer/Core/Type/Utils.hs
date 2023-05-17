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
      TFun m s t -> TFun m <$> go bound f s <*> go bound f t
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
    go (TFun _ a b) (TFun _ c d) = go a c && go b d
    go (TApp _ a b) (TApp _ c d) = go a c && go b d
    go _ _ = False
