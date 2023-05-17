module Primer.Core.Utils (
  freshLocalName,
  freshLocalName',
  forgetTypeMetadata,
  generateIDs,
  forgetMetadata,
  noHoles,
  _freeVarsTy,
  freeVarsTy,
  alphaEqTy,
) where

import Foreword

import Control.Monad.Fresh (MonadFresh, fresh)
import Optics (
  set,
  traverseOf,
 )

import Primer.Core (
  Expr,
  Expr' (..),
  ID,
  trivialMeta,
  _exprMeta,
  _exprTypeMeta,
 )
import Primer.Core.Fresh (freshLocalName, freshLocalName')
import Primer.Core.Type.Utils (
  alphaEqTy,
  forgetTypeMetadata,
  freeVarsTy,
  noHoles,
  _freeVarsTy,
 )

regenerateExprIDs' :: MonadFresh ID m => (ID -> a -> a') -> (ID -> b -> b') -> Expr' a b -> m (Expr' a' b')
regenerateExprIDs' se st =
  traverseOf _exprMeta (\a -> flip se a <$> fresh)
    >=> traverseOf _exprTypeMeta (\a -> flip st a <$> fresh)

-- | Like 'generateTypeIDs', but for expressions
generateIDs :: MonadFresh ID m => Expr' () () -> m Expr
generateIDs = regenerateExprIDs' (const . trivialMeta) (const . trivialMeta)

-- | Like 'forgetTypeMetadata', but for expressions
forgetMetadata :: Expr' a b -> Expr' () ()
forgetMetadata = set _exprTypeMeta () . set _exprMeta ()
