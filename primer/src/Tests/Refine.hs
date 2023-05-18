module Tests.Refine where

import Prelude

import Hedgehog (
  Property,
  discard,
  (===), property, forAll, Gen,
 )
import qualified Hedgehog.Gen as Gen
import Data.Maybe (catMaybes)

tasty_refinement_synths :: Property
tasty_refinement_synths = property $ do
  tgt <- forAll $ genWTType KType
  src <- forAll $ genWTType KType
  case refine tgt src of
    Just instTy -> do
      src === instTy
    _ -> discard

genWTType :: Kind -> Gen Type
genWTType k = do
  Gen.recursive Gen.choice [ehole] $ app : catMaybes [arrow]
  where
    ehole = pure TEmptyHole
    app = do TApp <$> genWTType (KFun KType k) <*> genWTType KType
    arrow =
      if k == KHole || k == KType
        then Just $ TFun <$> genWTType KType <*> genWTType KType
        else Nothing

data Type
  = TEmptyHole
  | TFun Type Type
  | TApp Type Type
  deriving stock (Eq, Show)

data Kind = KHole | KType | KFun Kind Kind
  deriving stock (Eq, Show)

consistentTypes :: Type -> Type -> Bool
consistentTypes TEmptyHole _ = True
consistentTypes _ TEmptyHole = True
consistentTypes (TFun s1 t1) (TFun s2 t2) = consistentTypes s1 s2 && consistentTypes t1 t2
consistentTypes (TApp s1 t1) (TApp s2 t2) = consistentTypes s1 s2 && consistentTypes t1 t2
consistentTypes _ _ = False


refine :: Type -> Type -> Maybe Type
refine tgtTy tmTy = if consistentTypes tgtTy tmTy
          then Just tmTy
          else case tmTy of
                 TFun _ t -> refine tgtTy t
                 _ -> Nothing
