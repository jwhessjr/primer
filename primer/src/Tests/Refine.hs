module Tests.Refine where

import Foreword hiding (diff)

import Control.Monad.Fresh (MonadFresh)
import Data.Map qualified as M
import Hedgehog (
  Property,
  annotateShow,
  diff,
  discard,
  (===),
 )
import Primer.Core (
  Expr' (Ann, EmptyHole),
  ID,
  Kind (KType),
 )
import Primer.Core.Utils (forgetMetadata, generateIDs)
import Primer.Gen.Core.Typed (
  propertyWT,
   forAllT,
  genInstApp,
  genWTType,
  synthTest,
 )
import Primer.Name (NameCounter)
import Primer.Refine (Inst (InstUnconstrainedAPP), refine)
import Primer.Subst (substTySimul)
import Primer.Typecheck (
  Cxt,
  Type,
  consistentTypes,
 )

refine' :: (MonadFresh NameCounter m, MonadFresh ID m) => Cxt -> Type -> Type -> m (Maybe ([Inst], Type))
refine' cxt s t = fmap (either crash identity) $ runExceptT $ refine cxt s t
  where
    -- If we run across a bug whilst testing, crash loudly
    crash = panic . ("InternalUnifyError: " <>) . show

-- if refine cxt tgt s = Just (is,ty)   =>  (? : s) $ <stuff checking against is>  ∈ ty[instantiation vars substituted appropriately] ~ tgt
tasty_refinement_synths :: Property
tasty_refinement_synths = propertyWT $ do
  tgt <- forAllT $ genWTType KType
  src <- forAllT $ genWTType KType
  cxt <- ask
  r <- refine' cxt tgt src
  annotateShow r
  case r of
    Just (is, instTy) -> do
      --(sb, apps) <- forAllT $ genInstApp is
      --let f x = \case Right tm -> App () x tm; Left ty' -> APP () x ty'
      --    e = foldl' f (Ann () (EmptyHole ()) src) apps
      let sb = mempty
      let apps = []
      let e = Ann () (EmptyHole ()) src
      --annotateShow e
      (ty, e') <- synthTest =<< generateIDs e
      e === forgetMetadata e' -- check no smart holes stuff happened
      let g i a = case (i, a) of (InstUnconstrainedAPP n _, Left t) -> Just $ M.singleton n t; _ -> Nothing
          sb' = mconcat $ catMaybes $ zipWith g is apps
      -- Check some invariants from @genInstApp@
      sb === sb'
      instTy' <- substTySimul sb instTy
      ty === instTy'
      diff ty consistentTypes tgt
    _ -> discard
