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
