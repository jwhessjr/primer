module ProgError (ProgError (..)) where

import Foreword

import Errors (ActionError)

data ProgError
  = NoDefSelected
  | DefNotFound Text
  | DefAlreadyExists Text
  | DefInUse Text
  | ActionError ActionError
  | -- | Currently copy/paste is only exposed in the frontend via select
    --   channels, which should never go wrong. Consequently, this is an
    --   "internal error" which should never happen!
    --   If/when we expose it more broadly, we should refactor this to contain
    --   a descriptive ADT, rather than a string.
    CopyPasteError Text
  deriving stock (Eq, Show, Read)
