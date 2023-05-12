module Primer.EvalFull (
  Dir, TerminationBound
) where

import Numeric.Natural (Natural)
import Primer.Eval.Redex (
  Dir
 )

-- Currently just a step limit
type TerminationBound = Natural
