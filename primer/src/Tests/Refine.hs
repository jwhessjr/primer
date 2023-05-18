module Tests.Refine where

import Prelude

import Hedgehog (
  Property,
  annotateShow,
  diff,
  discard,
  (===), property, failure,
 )
import Primer.Core (
  Expr (Ann, EmptyHole),
 )
import Primer.Core.Type (
  Kind (KType),
 )
import Primer.Gen.Core.Typed (
   forAllT,
  genWTType,
 )
import Primer.Refine (refine)
import Primer.Typecheck (
  consistentTypes, TypeError,
  synth
 )
import Control.Monad.Trans.Except (runExceptT)

-- if refine cxt tgt s = Just (is,ty)   =>  (? : s) $ <stuff checking against is>  ∈ ty[instantiation vars substituted appropriately] ~ tgt
tasty_refinement_synths :: Property
tasty_refinement_synths = property $ do
  tgt <- forAllT $ genWTType KType
  src <- forAllT $ genWTType KType
  let r = refine tgt src
  annotateShow r
  case r of
    Just instTy -> do
      --(sb, apps) <- forAllT $ genInstApp is
      --let f x = \case Right tm -> App () x tm; Left ty' -> APP () x ty'
      --    e = foldl' f (Ann () (EmptyHole ()) src) apps
      let e = Ann EmptyHole src
      --annotateShow e
      (ty, e') <- runExceptT @TypeError (synth e) >>= \case
        Left err -> annotateShow err >> failure
        Right y -> pure y
      e === e' -- check no smart holes stuff happened
      -- Check some invariants from @genInstApp@
      ty === instTy
      diff ty consistentTypes tgt
    _ -> discard
