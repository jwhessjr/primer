module Primer.Core (
  Expr (..),
  CaseBranch (..),
) where

import Foreword

import Data.Data (Data)
import Data.Generics.Uniplate.Data ()
import Primer.Core.Type (
  Type (..),
 )

data Expr
  = EmptyHole
  | Ann Expr Type
  | Case Expr [CaseBranch]
  deriving stock (Eq, Show, Read, Data, Generic)

data CaseBranch = CaseBranch
  deriving stock (Eq, Show, Read, Data, Generic)
