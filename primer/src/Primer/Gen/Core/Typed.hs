module Primer.Gen.Core.Typed (
  genWTType,
  genWTKind,
  forAllT,
) where

import Prelude

import Hedgehog (
  GenT,
 )
import Hedgehog.Gen qualified as Gen
import Hedgehog.Internal.Property (forAllT)
import Primer.Typecheck (
  Kind (..),
  Type (..)
 )
import Data.Maybe (catMaybes)

genWTType :: Monad m => Kind -> GenT m Type
genWTType k = do
  let rec = app : catMaybes [arrow]
  Gen.recursive Gen.choice [ehole] rec
  where
    ehole = pure $ TEmptyHole
    app = do k' <- genWTKind; TApp <$> genWTType (KFun k' k) <*> genWTType k'
    arrow =
      if k == KHole || k == KType
        then Just $ TFun <$> genWTType KType <*> genWTType KType
        else Nothing

genWTKind :: Monad m => GenT m Kind
genWTKind = Gen.recursive Gen.choice [pure KType] [KFun <$> genWTKind <*> genWTKind]
