-- |
-- This module generates "raw" terms and types.
-- That is, syntax trees which are not (necessarily) well-typed, or even well-scoped.
-- It is however, fast and has good coverage properties.
--
-- For generating well-typed terms, see "Primer.Gen.Core.Typed".
module Primer.Gen.Core.Raw (
  ExprGen,
  genName,
  genModuleName,
  genLVarName,
  genTyVarName,
) where

import Foreword

import Hedgehog hiding (Var, check)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Primer.Core (
  ID (..),
  LVarName,
  LocalName (LocalName),
  ModuleName (ModuleName),
  TyVarName,
 )
import Primer.Name (Name, unsafeMkName)

type ExprGen a = StateT ID Gen a

genModuleName :: MonadGen m => m ModuleName
genModuleName =
  ModuleName
    <$> Gen.frequency
      [ (9, pure $ "M" :| [])
      , (1, Gen.nonEmpty (Range.linear 1 3) genName)
      ]

genName :: MonadGen m => m Name
genName = unsafeMkName <$> Gen.frequency [(9, fixed), (1, random)]
  where
    fixed = Gen.element ["x", "y", "z", "foo", "bar"]
    random = Gen.text (Range.linear 1 10) Gen.alpha

genLVarName :: MonadGen m => m LVarName
genLVarName = LocalName <$> genName

genTyVarName :: MonadGen m => m TyVarName
genTyVarName = LocalName <$> genName
