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
  GVarName,
  Type,
  Type',
 )
import Primer.Core.Utils (forgetTypeMetadata)

data Def
  = DefAST ASTDef
  deriving stock (Eq, Show, Read, Data)


defType :: Def -> Type' ()
defType = \case
  DefAST d -> forgetTypeMetadata $ astDefType d

-- | A mapping of global names to 'Def's.
type DefMap = Map GVarName Def

-- | A top-level definition, built from an 'Expr'
data ASTDef = ASTDef
  { astDefExpr :: Expr
  , astDefType :: Type
  }
  deriving stock (Eq, Show, Read, Data)
