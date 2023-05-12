{-# LANGUAGE DuplicateRecordFields #-}

module Primer.Eval.Ann (RemoveAnnDetail (..)) where

import Foreword

import Primer.Core (
  Expr,
  ID,
 )

data RemoveAnnDetail = RemoveAnnDetail
  { before :: Expr
  -- ^ the expression before reduction
  , after :: Expr
  -- ^ the resulting expression after reduction
  , typeID :: ID
  -- ^ the ID of the type annotation
  }
  deriving stock (Eq, Show, Read, Generic)
