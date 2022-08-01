import Foreword (Applicative (pure), Foldable, ($))

import Control.Monad.Fresh (MonadFresh)
import Data.Foldable (foldl1)
import Primer.Builtins qualified as B
import Primer.Core (
  ASTDef (ASTDef),
  Def (DefAST),
  Expr,
  GVarName,
  ID,
  ModuleName,
  mkSimpleModuleName,
  qualifyName,
 )
import Primer.Core.DSL (
  app,
  branch,
  case_,
  con,
  gvar,
  lam,
  lvar,
  tcon,
  tfun,
 )

modName :: ModuleName
modName = mkSimpleModuleName "Prelude"

not :: GVarName
not = qualifyName modName "not"

-- | The function `not :: Bool -> Bool`.
notDef :: MonadFresh ID m => m Def
notDef = do
  type_ <- tcon B.tBool `tfun` tcon B.tBool
  term <-
    lam
      "x"
      ( case_
          (lvar "x")
          [ branch B.cTrue [] (con B.cFalse)
          , branch B.cFalse [] (con B.cTrue)
          ]
      )
  pure $ DefAST $ ASTDef term type_

and :: GVarName
and = qualifyName modName "and"

andDef :: MonadFresh ID m => m Def
andDef = do
  type_ <- tcon B.tBool `tfun` (tcon B.tBool `tfun` tcon B.tBool)
  term <-
    lam
      "x"
      ( case_
          (lvar "x")
          [ branch
              B.cTrue
              []
              ( lam
                  "y"
                  ( case_
                      (lvar "y")
                      [ branch B.cTrue [] (con B.cTrue)
                      , branch B.cFalse [] (con B.cFalse)
                      ]
                  )
              )
          , branch B.cFalse [] (lam "y" $ con B.cFalse)
          ]
      )
  pure $ DefAST $ ASTDef term type_

or :: GVarName
or = qualifyName modName "or"

orDef :: MonadFresh ID m => m Def
orDef = do
  type_ <- tcon B.tBool `tfun` (tcon B.tBool `tfun` tcon B.tBool)
  term <-
    lam
      "x"
      ( case_
          (lvar "x")
          [ branch B.cTrue [] (lam "y" $ con B.cTrue)
          , branch
              B.cFalse
              []
              ( lam
                  "y"
                  ( case_
                      (lvar "y")
                      [ branch B.cTrue [] $ con B.cTrue
                      , branch B.cFalse [] $ con B.cFalse
                      ]
                  )
              )
          ]
      )
  pure $ DefAST $ ASTDef term type_

-- | Helper function for functions with multiple arguments
-- Unsafe - do not call with empty list
appFold :: (Foldable t, MonadFresh ID m) => t (m Expr) -> m Expr
appFold = foldl1 app

xor :: GVarName
xor = qualifyName modName "xor"

xorDef :: MonadFresh ID m => m Def
xorDef = do
  type_ <- tcon B.tBool `tfun` (tcon B.tBool `tfun` tcon B.tBool)
  term <-
    lam
      "x"
      ( lam
          "y"
          ( appFold
              [ gvar and
              , appFold [gvar or, lvar "x", lvar "y"]
              , app (gvar not) (appFold [gvar and, lvar "x", lvar "y"])
              ]
          )
      )
  pure $ DefAST $ ASTDef term type_