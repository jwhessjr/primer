module Primer.Refine (refine) where

import Foreword

import Primer.Core.Type (
  Type' (TApp, TEmptyHole, TFun),
 )

refine :: Type' () -> Type' () -> Maybe (Type' ())
refine tgtTy = go
  where
    go tmTy = case unify tgtTy tmTy of
            Just _ -> Just tmTy
            Nothing -> case tmTy of
              TFun _ _ t -> go t
              _ -> Nothing

unify :: Type' () -> Type' () -> Maybe ()
unify (TEmptyHole _) _ = pure ()
unify _ (TEmptyHole _) = pure ()
unify (TFun _ s1 t1) (TFun _ s2 t2) = unify s1 s2 >> unify t1 t2
unify (TApp _ s1 t1) (TApp _ s2 t2) = unify s1 s2 >> unify t1 t2
unify _ _ = Nothing
