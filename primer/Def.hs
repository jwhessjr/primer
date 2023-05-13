module Def (
  Def (..),
  _DefAST,
  DefMap,
  ASTDef (..),
  _astDefExpr,
  _astDefType,
  defAST,
  defType,
) where

import Foreword

import Core (
  Expr,
  Type,
  Type',
 )
import CoreUtils (forgetTypeMetadata)
import Data.Data (Data)
import Optics (Iso', Lens', iso, lens)

data Def
  = DefAST ASTDef
  deriving stock (Eq, Show, Read, Data)

_DefAST :: Iso' Def ASTDef
_DefAST = iso (\(DefAST d) -> d) DefAST

defType :: Def -> Type' ()
defType = \case
  DefAST d -> forgetTypeMetadata $ astDefType d

-- | A mapping of global names to 'Def's.
type DefMap = Map Text Def

-- | A top-level definition, built from an 'Expr'
data ASTDef = ASTDef
  { astDefExpr :: Expr
  , astDefType :: Type
  }
  deriving stock (Eq, Show, Read, Data)

_astDefExpr :: Lens' ASTDef Expr
_astDefExpr = lens astDefExpr $ \d e -> d{astDefExpr = e}

_astDefType :: Lens' ASTDef Type
_astDefType = lens astDefType $ \d t -> d{astDefType = t}

defAST :: Def -> Maybe ASTDef
defAST = \case
  DefAST t -> Just t
