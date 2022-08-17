module Primer.Prelude.Integer (max, maxDef, min, minDef, negate, negateDef, abs, absDef) where

import Control.Monad.Fresh (MonadFresh)
import Foreword (Applicative (pure), ($))
import Primer.Builtins qualified as B
import Primer.Core (GVarName, ID, qualifyName)
import Primer.Core.DSL (app, branch, case_, gvar, int, lam, lvar, tcon, tfun)
import Primer.Def (ASTDef (..), Def (..))
import Primer.Prelude.Utils (apps, modName)
import Primer.Primitives (PrimDef (..), primDefName, primitiveGVar, tInt)

min :: GVarName
min = qualifyName modName "min"

minDef :: MonadFresh ID m => m Def
minDef = do
  type_ <- tcon tInt `tfun` (tcon tInt `tfun` tcon tInt)
  term <-
    lam
      "x"
      ( lam
          "y"
          ( case_
              (apps (gvar $ primitiveGVar $ primDefName IntLTE) [lvar "x", lvar "y"])
              [ branch B.cTrue [] $ lvar "x"
              , branch B.cFalse [] $ lvar "y"
              ]
          )
      )
  pure $ DefAST $ ASTDef term type_

max :: GVarName
max = qualifyName modName "max"

maxDef :: MonadFresh ID m => m Def
maxDef = do
  type_ <- tcon tInt `tfun` (tcon tInt `tfun` tcon tInt)
  term <-
    lam
      "x"
      ( lam
          "y"
          ( case_
              (apps (gvar $ primitiveGVar $ primDefName IntLTE) [lvar "x", lvar "y"])
              [ branch B.cTrue [] $ lvar "y"
              , branch B.cFalse [] $ lvar "x"
              ]
          )
      )
  pure $ DefAST $ ASTDef term type_

negate :: GVarName
negate = qualifyName modName "negate"

negateDef :: MonadFresh ID m => m Def
negateDef = do
  type_ <- tcon tInt `tfun` tcon tInt
  term <-
    lam
      "x"
      (apps (gvar $ primitiveGVar $ primDefName IntMinus) [int 0, lvar "x"])
  pure $ DefAST $ ASTDef term type_

abs :: GVarName
abs = qualifyName modName "abs"

absDef :: MonadFresh ID m => m Def
absDef = do
  type_ <- tcon tInt `tfun` tcon tInt
  term <-
    lam
      "x"
      (apps (gvar max) [lvar "x", app (gvar negate) (lvar "x")])
  pure $ DefAST $ ASTDef term type_
