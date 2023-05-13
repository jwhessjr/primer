module DefUtils (globalInUse) where

import Foreword

import CoreUtils (freeGlobalVars)
import Data.Set qualified as Set
import Def (Def (..), _DefAST, _astDefExpr)
import Meta (GVarName)
import Optics (anyOf, folded, to, (%))

globalInUse :: Foldable f => GVarName -> f Def -> Bool
globalInUse v =
  anyOf
    (folded % _DefAST % _astDefExpr % to freeGlobalVars)
    (Set.member v)
