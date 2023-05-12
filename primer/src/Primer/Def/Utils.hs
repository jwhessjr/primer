{-# LANGUAGE OverloadedLabels #-}

module Primer.Def.Utils (globalInUse) where

import Foreword

import Data.Set qualified as Set
import Optics (anyOf, folded, to, (%))
import Primer.Core.Meta (GVarName)
import Primer.Core.Utils (freeGlobalVars)
import Primer.Def (ASTDef (..), Def (..))

globalInUse :: Foldable f => GVarName -> f Def -> Bool
globalInUse v =
  anyOf
    (folded % #_DefAST % #astDefExpr % to freeGlobalVars)
    (Set.member v)
