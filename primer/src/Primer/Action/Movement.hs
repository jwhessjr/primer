module Primer.Action.Movement (Movement (..)) where

import Foreword

import Primer.Core.Meta (ValConName)
import Primer.JSON (CustomJSON (..), FromJSON, PrimerJSON, ToJSON)

-- | Core movements
data Movement
  = Child1
  | Child2
  | Parent
  | Branch ValConName
  | -- | Move into a term argument of a saturated constructor (zero-indexed)
    ConChild Int
  deriving stock (Eq, Show, Read, Generic)
  deriving (FromJSON, ToJSON) via PrimerJSON Movement
  deriving anyclass (NFData)
