module ProgError (ProgError (..)) where

import Foreword

import Errors (ActionError)
import Meta (GVarName, ModuleName)

data ProgError
  = NoDefSelected
  | DefNotFound GVarName
  | DefAlreadyExists GVarName
  | DefInUse GVarName
  | ActionError ActionError
  | -- | Currently copy/paste is only exposed in the frontend via select
    --   channels, which should never go wrong. Consequently, this is an
    --   "internal error" which should never happen!
    --   If/when we expose it more broadly, we should refactor this to contain
    --   a descriptive ADT, rather than a string.
    CopyPasteError Text
  | ModuleNotFound ModuleName
  deriving stock (Eq, Show, Read)
