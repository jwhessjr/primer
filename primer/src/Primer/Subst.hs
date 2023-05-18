module Primer.Subst (
  substTySimul,
) where

import Foreword

import Control.Monad.Fresh (MonadFresh)
import Data.Map qualified as M
import Primer.Core.Meta (TyVarName)
import Primer.Core.Type (Type' (..))
import Primer.Name (NameCounter)

-- | Simple and inefficient capture-avoiding simultaneous substitution.
-- @substTySimul [(a,A),(b,B)] t@ is @t[A,B/a,b]@, where any references to @a,b@
-- in their replacements @A,B@ are not substituted.
-- We restrict to '()', i.e. no metadata as we don't want to duplicate IDs etc
substTySimul :: MonadFresh NameCounter m => Map TyVarName (Type' ()) -> Type' () -> m (Type' ())
substTySimul sub
  | M.null sub = pure
  | otherwise = go
  where
    go = \case
      t@TEmptyHole{} -> pure t
      TFun _ s t -> TFun () <$> go s <*> go t
      TApp _ s t -> TApp () <$> go s <*> go t
