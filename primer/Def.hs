module Def (
  Def (..),
  DefMap,
  _defExpr,
  _defType,
) where

import Foreword

import Core (
  Expr,
  Type,
 )
import Data.Data (Data)
import Optics (Lens', lens)

data Def
  = Def
  { defExpr :: Expr
  , defType :: Type
  }
  deriving stock (Eq, Show, Read, Data)

-- | A mapping of global names to 'Def's.
type DefMap = Map Text Def

_defExpr :: Lens' Def Expr
_defExpr = lens defExpr $ \d e -> d{defExpr = e}

_defType :: Lens' Def Type
_defType = lens defType $ \d t -> d{defType = t}
