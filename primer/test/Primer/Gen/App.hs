-- ApplicativeDo: generators shrink much better if applicative (though much of
-- this module is inherently monadic)
{-# LANGUAGE ApplicativeDo #-}

-- |
-- This module generates well-typed 'Prog's
-- It is however, slow and the distribution is not very even.
module Primer.Gen.App (
  extendCxtByModules,
) where

import Primer.Core.Utils (forgetTypeMetadata)
import Primer.Def (defType)
import Primer.Module (Module, moduleDefsQualified, moduleTypesQualified)
import Primer.Typecheck (
  Cxt,
  extendGlobalCxt,
  extendTypeDefCxt,
 )



import Data.Map qualified as M

import Foreword hiding (mod)

extendCxtByModules :: [Module] -> Cxt -> Cxt
extendCxtByModules ms =
      extendTypeDefCxt (foldMap' moduleTypesQualified ms)
        . extendGlobalCxt (M.toList . fmap (forgetTypeMetadata . defType) $ foldMap' moduleDefsQualified ms)
