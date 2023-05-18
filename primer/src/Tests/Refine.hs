module Tests.Refine where

import Foreword hiding (diff)

import Hedgehog (
  Property,
  annotateShow,
  diff,
  discard,
  (===),
 )
import Primer.Core (
  Expr' (Ann, EmptyHole),
  Kind (KType),
 )
import Primer.Core.Utils (forgetMetadata, generateIDs)
import Primer.Gen.Core.Typed (
  propertyWT,
   forAllT,
  genWTType,
  synthTest,
 )
import Primer.Refine (refine)
import Primer.Subst (substTySimul)
import Primer.Typecheck (
  consistentTypes,
 )

-- if refine cxt tgt s = Just (is,ty)   =>  (? : s) $ <stuff checking against is>  ∈ ty[instantiation vars substituted appropriately] ~ tgt
tasty_refinement_synths :: Property
tasty_refinement_synths = propertyWT $ do
  tgt <- forAllT $ genWTType KType
  src <- forAllT $ genWTType KType
  let r = refine tgt src
  annotateShow r
  case r of
    Just instTy -> do
      --(sb, apps) <- forAllT $ genInstApp is
      --let f x = \case Right tm -> App () x tm; Left ty' -> APP () x ty'
      --    e = foldl' f (Ann () (EmptyHole ()) src) apps
      let sb = mempty
      let e = Ann () (EmptyHole ()) src
      --annotateShow e
      (ty, e') <- synthTest =<< generateIDs e
      e === forgetMetadata e' -- check no smart holes stuff happened
      let sb' = mempty
      -- Check some invariants from @genInstApp@
      sb === sb'
      instTy' <- substTySimul sb instTy
      ty === instTy'
      diff ty consistentTypes tgt
    _ -> discard
