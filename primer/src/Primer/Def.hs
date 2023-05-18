module Primer.Def (
  Def (..),
  DefMap,
  ASTDef (..),
  defType,
) where

import Foreword

import Data.Data (Data)
import Primer.Core (
  Expr,
 )
import Primer.Core.Meta (
  GVarName,
 )
import Primer.Core.Type (
  Type,
 )

data Def
  = DefAST ASTDef
  deriving stock (Eq, Show, Read, Data)


defType :: Def -> Type
defType = \case
  DefAST d -> astDefType d

-- | A mapping of global names to 'Def's.
type DefMap = Map GVarName Def

-- | A top-level definition, built from an 'Expr'
data ASTDef = ASTDef
  { astDefExpr :: Expr
  , astDefType :: Type
  }
  deriving stock (Eq, Show, Read, Data)
