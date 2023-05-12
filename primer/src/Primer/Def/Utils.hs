{-# LANGUAGE OverloadedLabels #-}

module Primer.Def.Utils (globalInUse) where

import Foreword

import Data.Set qualified as Set
import Optics (anyOf, folded, to, (%))
import Primer.Core.Meta (GVarName)
import Primer.Core.Utils (freeGlobalVars)
import Primer.Def (Def (..), _DefAST, _astDefExpr)

globalInUse :: Foldable f => GVarName -> f Def -> Bool
globalInUse v =
  anyOf
    (folded % _DefAST % _astDefExpr % to freeGlobalVars)
    (Set.member v)
