module Primer.Refine (refine) where

import Foreword

import Primer.Core.Type (
  Type' (TApp, TEmptyHole, TFun),
 )
import Primer.Typecheck.Kindcheck (
  Type,
 )
import Primer.Typecheck.Kindcheck qualified as TC

-- | Given a target type @T@ and a source type @S@, find an instantiation @I@
-- so that if @e ∈ S@, then @e I ∈ T' ~ T@
-- Here,
--
--  * @e (InstApp ty)@ represents "apply to any @t@ st @ty ∋ t@"
--  * @e (InstAPP ty)@ represents "apply to the type @ty@: @e \@ty@"
--  * @e (InstUnconstrainedAPP _ k)@ represents "apply to some type of kind @k@, but we don't care what"
--
-- The names in @InstUnconstrainedAPP@s are all unique, and they scope over all
-- the @Inst@s to the right, as well as the returned @Type@.
refine :: TC.Type -> TC.Type -> Maybe TC.Type
refine tgtTy = go
  where
    go :: TC.Type -> Maybe TC.Type
    go tmTy = case unify tgtTy tmTy of
            Just _ -> Just tmTy
            Nothing -> case tmTy of
              TFun _ _ t -> go t
              _ -> Nothing


unify :: Type -> Type -> Maybe ()
unify (TEmptyHole _) _ = pure ()
unify _ (TEmptyHole _) = pure ()
unify (TFun _ s1 t1) (TFun _ s2 t2) = unify s1 s2 >> unify t1 t2
-- Doing first-order unification, as applications are only constructor-like
-- (we don't have any bona fide functions at the type level)
unify (TApp _ s1 t1) (TApp _ s2 t2) = unify s1 s2 >> unify t1 t2
unify _ _ = Nothing
