module Tests.Refine where

import Prelude

import Hedgehog (
  Property,
  discard,
  (===), property, forAll,
 )
import Primer.Gen.Core.Typed (
  genWTType,
 )
import Primer.Typecheck (
  Kind (KType),refine
 )

tasty_refinement_synths :: Property
tasty_refinement_synths = property $ do
  tgt <- forAll $ genWTType KType
  src <- forAll $ genWTType KType
  case refine tgt src of
    Just instTy -> do
      src === instTy
    _ -> discard
