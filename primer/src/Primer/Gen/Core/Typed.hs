module Primer.Gen.Core.Typed (
  genWTType,
) where

import Prelude

import Hedgehog (
  GenT,
 )
import Hedgehog.Gen qualified as Gen
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
    app = do TApp <$> genWTType (KFun KType k) <*> genWTType KType
    arrow =
      if k == KHole || k == KType
        then Just $ TFun <$> genWTType KType <*> genWTType KType
        else Nothing
