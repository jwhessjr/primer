module Primer.Eval.Redex (
  Dir,
) where

import Foreword

import Primer.JSON (CustomJSON (CustomJSON), FromJSON, PrimerJSON, ToJSON)

data Dir = Syn | Chk
  deriving stock (Eq, Show, Read, Generic)
  deriving (FromJSON, ToJSON) via PrimerJSON Dir
