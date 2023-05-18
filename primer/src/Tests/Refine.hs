module Tests.Refine where

import Prelude

import Hedgehog (
  Property,
  annotateShow,
  discard,
  (===), property, failure, forAll,
 )
import Primer.Gen.Core.Typed (
  genWTType,
 )
import Primer.Typecheck (
  Expr (Ann, EmptyHole),
  Kind (KType),
  TypeError,
  synth,refine
 )
import Control.Monad.Trans.Except (runExceptT)

tasty_refinement_synths :: Property
tasty_refinement_synths = property $ do
  tgt <- forAll $ genWTType KType
  src <- forAll $ genWTType KType
  case refine tgt src of
    Just instTy -> do
      (ty, _) <- runExceptT @TypeError (synth $ Ann EmptyHole src) >>= \case
        Left err -> annotateShow err >> failure
        Right y -> pure y
      ty === instTy
    _ -> discard
