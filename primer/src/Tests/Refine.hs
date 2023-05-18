module Tests.Refine where

import Prelude

import Hedgehog (
  Property,
  annotateShow,
  diff,
  discard,
  (===), property, failure,
 )
import Primer.Gen.Core.Typed (
   forAllT,
  genWTType,
 )
import Primer.Typecheck (
  Expr (Ann, EmptyHole),
  Kind (KType),
  consistentTypes, TypeError,
  synth,refine
 )
import Control.Monad.Trans.Except (runExceptT)

tasty_refinement_synths :: Property
tasty_refinement_synths = property $ do
  tgt <- forAllT $ genWTType KType
  src <- forAllT $ genWTType KType
  case refine tgt src of
    Just instTy -> do
      let e = Ann EmptyHole src
      (ty, e') <- runExceptT @TypeError (synth e) >>= \case
        Left err -> annotateShow err >> failure
        Right y -> pure y
      e === e'
      ty === instTy
      diff ty consistentTypes tgt
    _ -> discard
