module DefUtils (globalInUse) where

import Foreword

import CoreUtils (freeGlobalVars)
import Data.Set qualified as Set
import Def (Def (..), _defExpr)
import Optics (anyOf, folded, to, (%))

globalInUse :: Foldable f => Text -> f Def -> Bool
globalInUse v =
  anyOf
    (folded % _defExpr % to freeGlobalVars)
    (Set.member v)
