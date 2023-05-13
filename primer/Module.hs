module Module (
  Module (..),
  _moduleDefs,
  qualifyTyConName,
  moduleTypesQualified,
  qualifyDefName,
  moduleDefsQualified,
  insertDef,
  deleteDef,
) where

import Foreword

import Data.Data (Data)
import Data.Map (delete, insert, mapKeys, member)
import Core (
  GVarName,
  GlobalName (baseName),
  ModuleName,
  TyConName,
  TypeMeta,
  qualifyName,
 )
import Def (
  Def (..),
  DefMap,
 )
import Name (Name)
import TypeDef (TypeDef (..), TypeDefMap, forgetTypeDefMetadata)
import Optics (Lens', lens)

data Module = Module
  { moduleName :: ModuleName
  , moduleTypes :: Map Name (TypeDef TypeMeta)
  , moduleDefs :: Map Name Def -- The current program: a set of definitions indexed by Name
  }
  deriving stock (Eq, Show, Read, Data)

_moduleDefs :: Lens' Module (Map Name Def)
_moduleDefs = lens moduleDefs (\m d -> m {moduleDefs = d})

qualifyTyConName :: Module -> Name -> TyConName
qualifyTyConName m = qualifyName (moduleName m)

moduleTypesQualified :: Module -> TypeDefMap
moduleTypesQualified m = mapKeys (qualifyTyConName m) $ forgetTypeDefMetadata <$> moduleTypes m

qualifyDefName :: Module -> Name -> GVarName
qualifyDefName m = qualifyName (moduleName m)

moduleDefsQualified :: Module -> DefMap
moduleDefsQualified m = mapKeys (qualifyDefName m) $ moduleDefs m

-- | This assumes that the definition has the correct name to be inserted
-- into the module. I.e. @qualifiedModule (defName d) == moduleName m@.
insertDef :: Module -> Name -> Def -> Module
insertDef m n d = m{moduleDefs = insert n d $ moduleDefs m}

-- | Returns 'Nothing' if (and only if) the definition was not found in the module
deleteDef :: Module -> GVarName -> Maybe Module
deleteDef m d =
  if d `member` moduleDefsQualified m
    then Just $ m{moduleDefs = delete (baseName d) (moduleDefs m)}
    else Nothing
