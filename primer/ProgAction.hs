module ProgAction (ProgAction (..)) where

-- We split this module to increase parallelism in our build.
-- This module does not depend on much, but takes a long time
-- to build because of
-- https://gitlab.haskell.org/ghc/ghc/-/issues/5642
--
-- It is split from Actions and ActionError
-- because this depends on ASTTypeDef

import Foreword

import Actions (Action)
import Meta (GVarName, ID)

-- | High level actions
-- These actions move around the whole program or modify definitions
data ProgAction
  = -- | Move the cursor to the definition with the given Name
    MoveToDef GVarName
  | -- | Rename the definition with the given (base) Name
    RenameDef GVarName Text
  | -- | Delete a new definition
    DeleteDef GVarName
  | -- | Execute a sequence of actions on the body of the definition
    BodyAction [Action]
  | -- | Execute a sequence of actions on the type annotation of the definition
    SigAction [Action]
  | -- | CopyPaste (d,i) as
    --   remembers the tree in def d, node i
    --   runs actions as (in the currently selected def), which should end up in a hole
    --   and then tries to paste the remembered subtree
    --   This rather complex setup enables encoding 'raise' operations,
    --     f s ~> f
    --   where we remember f, then delete f s, then paste f back
    --   as well as allowing cross-definition copy+paste
    --   whilst letting the backend avoid remembering the 'copied' thing in some state.
    --   The cursor is left on the root of the inserted subtree, which may or may not be inside a hole and/or annotation.
    --   At the start of the actions, the cursor starts at the root of the definition's type/expression
    CopyPasteSig (GVarName, ID) [Action]
  | CopyPasteBody (GVarName, ID) [Action]
  deriving stock (Eq, Show, Read)
