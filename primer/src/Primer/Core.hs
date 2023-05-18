module Primer.Core (
  Expr (..),
  CaseBranch (..),
) where

import Foreword

import Primer.Core.Type (
  Type (..),
 )

data Expr
  = EmptyHole
  | Ann Expr Type
  | Case Expr [CaseBranch]
  deriving stock (Eq, Show)

data CaseBranch = CaseBranch
  deriving stock (Eq, Show)
