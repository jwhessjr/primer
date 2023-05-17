module Tests.Refine where

import Foreword hiding (diff)

import Control.Monad.Fresh (MonadFresh)
import Data.Map qualified as M
import Hedgehog (
  Property,
  annotateShow,
  diff,
  discard,
  failure,
  success,
  (===),
 )
--import Primer.Builtins (tBool, tList, tNat)
import Primer.Core (
  Expr' (APP, Ann, App, EmptyHole),
  ID,
  Kind (KFun, KType),
  Type' (TApp, TCon, TEmptyHole, TForall, TFun, THole, TVar),
 )
--import Primer.Core.Utils (forgetMetadata, freeVarsTy, generateIDs, noHoles)
import Primer.Core.Utils (forgetMetadata, generateIDs)
import Primer.Gen.Core.Typed (
  propertyWT,
   forAllT,
  -- freshTyVarNameForCxt,
  genInstApp,
  -- genWTKind,
  genWTType,
  synthTest,
 )
-- import Primer.Module (builtinModule, primitiveModule)
import Primer.Name (NameCounter)
import Primer.Refine (Inst (InstAPP, InstApp, InstUnconstrainedAPP), refine)
import Primer.Subst (substTy, substTySimul)
import Primer.Test.TestM (evalTestM)
import Primer.TypeDef (astTypeDefConstructors, astTypeDefParameters, typeDefAST, valConType)
import Primer.Typecheck (
  Cxt,
  SmartHoles (NoSmartHoles),
  Type,
  buildTypingContextFromModules',
  consistentTypes,
  extendLocalCxtTy,
  mkTAppCon,
  typeDefs,
 )
--import Tasty (Property, withDiscards)
--import Test.Tasty.HUnit (Assertion, (@?=))
--import Tests.Gen.Core.Typed (propertyWTInExtendedLocalGlobalCxt, synthTest)

refine' :: (MonadFresh NameCounter m, MonadFresh ID m) => Cxt -> Type -> Type -> m (Maybe ([Inst], Type))
refine' cxt s t = fmap (either crash identity) $ runExceptT $ refine cxt s t
  where
    -- If we run across a bug whilst testing, crash loudly
    crash = panic . ("InternalUnifyError: " <>) . show

-- if refine cxt tgt s = Just (is,ty)   =>  (? : s) $ <stuff checking against is>  ∈ ty[instantiation vars substituted appropriately] ~ tgt
tasty_refinement_synths :: Property
tasty_refinement_synths = propertyWT [] $ do
  tgt <- forAllT $ genWTType KType
  src <- forAllT $ genWTType KType
  cxt <- ask
  r <- refine' cxt tgt src
  annotateShow r
  case r of
    Just (is, instTy) -> do
      (sb, apps) <- forAllT $ genInstApp is
      let f x = \case Right tm -> App () x tm; Left ty' -> APP () x ty'
          e = foldl' f (Ann () (EmptyHole ()) src) apps
      annotateShow e
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

isHole :: Type' a -> Bool
isHole (TEmptyHole _) = True
isHole (THole _ _) = True
isHole _ = False
