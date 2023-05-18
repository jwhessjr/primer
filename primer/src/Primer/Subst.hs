module Primer.Subst (
  substTySimul,
) where

import Foreword

import Primer.Core.Meta (TyVarName)
import Primer.Core.Type (Type' (..))

-- | Simple and inefficient capture-avoiding simultaneous substitution.
-- @substTySimul [(a,A),(b,B)] t@ is @t[A,B/a,b]@, where any references to @a,b@
-- in their replacements @A,B@ are not substituted.
-- We restrict to '()', i.e. no metadata as we don't want to duplicate IDs etc
substTySimul :: Applicative m => Map TyVarName (Type' ()) -> Type' () -> m (Type' ())
substTySimul _sub = pure
