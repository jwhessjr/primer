module Primer.Core.Utils (
  forgetTypeMetadata,
  generateIDs,
  forgetMetadata,
  alphaEqTy,
) where

import Foreword

import Primer.Core (
  Expr (..),
 )
import Primer.Core.Type.Utils (
  alphaEqTy,
  forgetTypeMetadata,
 )

-- | Like 'generateTypeIDs', but for expressions
generateIDs :: Applicative m => Expr -> m Expr
generateIDs = pure

-- | Like 'forgetTypeMetadata', but for expressions
forgetMetadata :: Expr -> Expr
forgetMetadata = identity
