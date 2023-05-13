module Module (
  Module (..),
  _moduleDefs,
  insertDef,
  deleteDef,
) where

import Foreword

import Core (
  GVarName,
  TypeMeta,
 )
import Data.Data (Data)
import Data.Map (delete, insert)
import Def (
  Def (..),
 )
import Optics (Lens', lens)
import TypeDef (TypeDef (..))

data Module = Module
  { moduleTypes :: Map Text (TypeDef TypeMeta)
  , moduleDefs :: Map Text Def -- The current program: a set of definitions indexed by Name
  }
  deriving stock (Eq, Show, Read, Data)

_moduleDefs :: Lens' Module (Map Text Def)
_moduleDefs = lens moduleDefs (\m d -> m{moduleDefs = d})

-- | This assumes that the definition has the correct name to be inserted
-- into the module. I.e. @qualifiedModule (defName d) == moduleName m@.
insertDef :: Module -> Text -> Def -> Module
insertDef m n d = m{moduleDefs = insert n d $ moduleDefs m}

deleteDef :: Module -> GVarName -> Module
deleteDef m d = m{moduleDefs = delete d (moduleDefs m)}
