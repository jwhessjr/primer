module Primer.Typecheck.SmartHoles (SmartHoles (..)) where

import Foreword

data SmartHoles = SmartHoles | NoSmartHoles
  deriving stock (Eq, Show, Read, Generic)
