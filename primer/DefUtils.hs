module DefUtils (globalInUse) where

import Foreword

import Data.Set qualified as Set
import Optics (anyOf, folded, to, (%))
import Meta (GVarName)
import CoreUtils (freeGlobalVars)
import Def (Def (..), _DefAST, _astDefExpr)

globalInUse :: Foldable f => GVarName -> f Def -> Bool
globalInUse v =
  anyOf
    (folded % _DefAST % _astDefExpr % to freeGlobalVars)
    (Set.member v)
