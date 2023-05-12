{-# LANGUAGE DuplicateRecordFields #-}

module Primer.Eval.Prim (ApplyPrimFunDetail) where

import Foreword

import Primer.Core (
  Expr,
  GVarName,
  ID,
 )

data ApplyPrimFunDetail = ApplyPrimFunDetail
  { before :: Expr
  -- ^ the expression before reduction
  , after :: Expr
  -- ^ the expression after reduction
  , name :: GVarName
  -- ^ the name of the primitive function
  , argIDs :: [ID]
  -- ^ the IDs of the arguments to the application
  }
  deriving stock (Eq, Show, Read, Generic)
