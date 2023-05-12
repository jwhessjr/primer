-- |
-- This module generates "raw" terms and types.
-- That is, syntax trees which are not (necessarily) well-typed, or even well-scoped.
-- It is however, fast and has good coverage properties.
--
-- For generating well-typed terms, see "Primer.Gen.Core.Typed".
module Primer.Gen.Core.Raw (
  ExprGen,
  genName,
) where

import Foreword

import Hedgehog hiding (Var, check)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Primer.Core (
  ID (..),
 )
import Primer.Name (Name, unsafeMkName)

type ExprGen a = StateT ID Gen a

genName :: MonadGen m => m Name
genName = unsafeMkName <$> Gen.frequency [(9, fixed), (1, random)]
  where
    fixed = Gen.element ["x", "y", "z", "foo", "bar"]
    random = Gen.text (Range.linear 1 10) Gen.alpha
