module CoreUtils (
  exprIDs,
  typeIDs,
  noHoles,
  freeGlobalVars,
  alphaEqTy,
) where

import Foreword

import Core (
  Type (..),
  Expr (..),
  HasID (_id),
  _exprMeta,
  _exprTypeMeta,
  _typeMeta,
 )
import Data.Generics.Uniplate.Data (universe)
import Optics (
  Traversal',
  adjoin,
  (%),
 )

-- | Test whether an type contains any holes
-- (empty or non-empty, or inside a kind)
noHoles :: Type -> Bool
noHoles t = flip all (universe t) $ \case
  TEmptyHole{} -> False
  _ -> True

-- Check two types for alpha equality, ignoring IDs
alphaEqTy :: Type -> Type -> Bool
alphaEqTy = go
  where
    go (TEmptyHole _) (TEmptyHole _) = True
    go (TCon _ n) (TCon _ m) = n == m
    go (TFun _ a b) (TFun _ c d) = go a c && go b d
    go _ _ = False

-- | Traverse the 'ID's in a 'Type''.
typeIDs :: Traversal' Type Int
typeIDs = _typeMeta

freeGlobalVars :: Expr -> Set Text
freeGlobalVars _ = mempty

-- | Traverse the 'ID's in an 'Expr''.
exprIDs :: Traversal' Expr Int
exprIDs = (_exprMeta % _id) `adjoin` _exprTypeMeta
