module Primer.Gen.Core.Raw (
  genName,
  genTyVarName,
) where

import Foreword

import Hedgehog ( MonadGen )
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Primer.Core (
  LocalName (LocalName),
  TyVarName,
 )
import Primer.Name (Name, unsafeMkName)

genName :: MonadGen m => m Name
genName = unsafeMkName <$> Gen.frequency [(9, fixed), (1, random)]
  where
    fixed = Gen.element ["x", "y", "z", "foo", "bar"]
    random = Gen.text (Range.linear 1 10) Gen.alpha

genTyVarName :: MonadGen m => m TyVarName
genTyVarName = LocalName <$> genName
