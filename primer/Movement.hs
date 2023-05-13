module Movement (Movement (..)) where

import Foreword

-- | Core movements
data Movement = Parent
  deriving stock (Eq, Show, Read)
