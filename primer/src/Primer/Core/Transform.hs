module Primer.Core.Transform (
  renameVar,
) where

import Foreword

import Data.Data (Data)
import Data.Generics.Uniplate.Data (descendM)
import Optics (Field2 (_2), getting, noneOf, to, (%))
import Primer.Core (
  CaseBranch' (..),
  Expr' (..),
  LocalName (unLocalName),
  TmVarRef (..),
  bindName,
  typesInExpr,
 )
import Primer.Core.Utils (_freeVars, _freeVarsTy)

-- AST transformations.
-- This module contains global transformations on expressions and types, in
-- contrast to the focused, local transformations provided by the zipper.

-- | Attempt to replace all free ocurrences of @x@ in @e@ with @y@
-- Returns 'Nothing' if replacement could result in variable capture.
-- See the tests for explanation and examples.
renameVar :: (Data a, Data b) => TmVarRef -> TmVarRef -> Expr' a b -> Maybe (Expr' a b)
renameVar x y expr = case expr of
  Lam _ v _
    | sameVarRef v x -> whenNotFreeIn y expr
    | sameVarRef v y -> Nothing
    | otherwise -> substAllChildren
  Case m scrut branches -> Case m <$> renameVar x y scrut <*> mapM renameBranch branches
    where
      renameBranch b@(CaseBranch con termargs rhs)
        | any (`sameVarRef` y) $ bindingNames b = Nothing
        | any (`sameVarRef` x) $ bindingNames b = guard (notFreeIn y rhs) >> pure b
        | otherwise = CaseBranch con termargs <$> renameVar x y rhs
      bindingNames (CaseBranch _ bs _) = map bindName bs
  Var m v
    | v == x -> pure $ Var m y
    | v == y -> Nothing
    | otherwise -> pure expr
  Hole{} -> substAllChildren
  EmptyHole{} -> substAllChildren
  Ann{} -> substAllChildren
  App{} -> substAllChildren
  -- We assume the term is well-scoped, so do not have any references to the
  -- term vars x,y inside any type child (e.g. annotation), so no need to
  -- consider renaming inside them. However, but we do need to worry about
  -- references to the type var y (term and type variables are in the same
  -- namespace) -- we do not want to capture such a y.
  where
    substAllChildren = do
      guard $ noneOf (typesInExpr % getting _freeVarsTy % _2) (`sameVarRef` y) expr
      descendM (renameVar x y) expr

whenNotFreeIn :: TmVarRef -> Expr' a b -> Maybe (Expr' a b)
whenNotFreeIn x e = do
  guard $ notFreeIn x e
  pure e

notFreeIn :: TmVarRef -> Expr' a b -> Bool
notFreeIn x = noneOf (_freeVars % to (bimap snd snd)) (either (`sameVarRef` x) (`sameVarRef` x))

sameVarRef :: LocalName k -> TmVarRef -> Bool
sameVarRef v (LocalVarRef v') = sameVar v v'
sameVarRef _ (GlobalVarRef _) = False

sameVar :: LocalName k -> LocalName l -> Bool
sameVar v v' = unLocalName v == unLocalName v'
