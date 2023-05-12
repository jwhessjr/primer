module Primer.Eval.Redex (
  Dir,
) where

import Foreword

data Dir = Syn | Chk
  deriving stock (Eq, Show, Read)
