module Primer.Core.Type.Utils (
  forgetTypeMetadata,
  alphaEqTy,
) where

import Foreword

import Optics (
  set,
 )

import Primer.Core.Type (
  Type' (..),
  _typeMeta,
 )

-- | Replace all 'ID's in a Type with unit.
-- Technically this replaces all annotations, regardless of what they are.
forgetTypeMetadata :: Type' a -> Type' ()
forgetTypeMetadata = set _typeMeta ()

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
