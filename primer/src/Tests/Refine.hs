module Tests.Refine where

import Prelude

import Data.Maybe (catMaybes)
import Hedgehog (
  Gen,
  Property,
  discard,
  forAll,
  property,
  (===),
 )
import Hedgehog.Gen qualified as Gen

tasty_replay_broken :: Property
tasty_replay_broken = property $ do
  tgt <- forAll $ genWTType KType
  src <- forAll $ genWTType KType
  case stripArgs tgt src of
    Just instTy -> do
      src === instTy
    _ -> discard

genWTType :: Kind -> Gen Type
genWTType k = do
  Gen.recursive Gen.choice [ehole] $ app : catMaybes [arrow]
  where
    ehole = pure TBase
    app = do TApp <$> genWTType (KFun KType k) <*> genWTType KType
    arrow =
      if k == KType
        then Just $ TFun <$> genWTType KType <*> genWTType KType
        else Nothing

data Type
  = TBase
  | TFun Type Type
  | TApp Type Type
  deriving stock (Eq, Show)

data Kind = KType | KFun Kind Kind
  deriving stock (Eq, Show)

stripArgs :: Type -> Type -> Maybe Type
stripArgs tgt ty =
  if tgt == ty
    then Just ty
    else case ty of
      TFun _ t -> stripArgs tgt t
      _ -> Nothing
