module Primer.Unification (unify) where

import Foreword

import Primer.Core.Type (
  Type' (TApp, TEmptyHole, TFun),
 )
import Primer.Typecheck.Kindcheck (
  Type,
 )

unify :: Type -> Type -> Maybe ()
unify (TEmptyHole _) _ = pure ()
unify _ (TEmptyHole _) = pure ()
unify (TFun _ s1 t1) (TFun _ s2 t2) = unify s1 s2 >> unify t1 t2
-- Doing first-order unification, as applications are only constructor-like
-- (we don't have any bona fide functions at the type level)
unify (TApp _ s1 t1) (TApp _ s2 t2) = unify s1 s2 >> unify t1 t2
unify _ _ = Nothing
