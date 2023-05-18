module Primer.Core.Type.Utils (
  forgetTypeMetadata,
  alphaEqTy,
) where

import Foreword

import Primer.Core.Type (
  Type (..),
 )

forgetTypeMetadata :: Type -> Type
forgetTypeMetadata = identity

-- Check two types for alpha equality
--
-- it makes usage easier if this is pure
-- i.e. we don't want to need a fresh name supply
-- We assume both inputs are both from the same context
--
-- Note that we do not expand TLets, they must be structurally
-- the same (perhaps with a different named binding)
alphaEqTy :: Type -> Type -> Bool
alphaEqTy = go
  where
    go TEmptyHole TEmptyHole = True
    go (TFun a b) (TFun c d) = go a c && go b d
    go (TApp a b) (TApp c d) = go a c && go b d
    go _ _ = False
