module Movement (Movement (..)) where

import Foreword

import Meta (ValConName)

-- | Core movements
data Movement = Child1 | Child2 | Parent | Branch ValConName
  deriving stock (Eq, Show, Read)
