module Primer.Module (
  Module (..),
  _moduleTypes,
  _moduleDefs,
  qualifyTyConName,
  moduleTypesQualified,
  qualifyDefName,
  moduleDefsQualified,
  insertDef,
  deleteDef,
  renameModule,
  renameModule',
) where

import Foreword

import Data.Data (Data)
import Data.Generics.Uniplate.Data (transformBi)
import Data.Map (delete, insert, mapKeys, member)
import Primer.Core (
  GVarName,
  GlobalName (baseName),
  ModuleName,
  TyConName,
  TypeMeta,
  qualifyName,
 )
import Primer.Def (
  Def (..),
  DefMap,
 )
import Primer.Name (Name)
import Primer.TypeDef (TypeDef (..), TypeDefMap, forgetTypeDefMetadata)
import Optics (Lens', lens)

data Module = Module
  { moduleName :: ModuleName
  , moduleTypes :: Map Name (TypeDef TypeMeta)
  , moduleDefs :: Map Name Def -- The current program: a set of definitions indexed by Name
  }
  deriving stock (Eq, Show, Read, Data)

_moduleTypes :: Lens' Module (Map Name (TypeDef TypeMeta))
_moduleTypes = lens moduleTypes (\m t -> m {moduleTypes = t})

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

-- | Renames a module and any references to it (in the given 'Traversable' of
-- modules). Returns 'Nothing' if the requested new name is in use
-- (as the name of one of the modules, references are not detected)
renameModule :: Traversable t => ModuleName -> ModuleName -> t Module -> Maybe (t Module)
renameModule fromName toName = traverse rn1
  where
    rn1 m =
      if moduleName m == toName
        then Nothing
        else pure $ renameModule' fromName toName m

-- | Renames all occurrences of the given 'ModuleName'. This does not
-- detect name clashes, see 'renameModule'
renameModule' :: Data a => ModuleName -> ModuleName -> a -> a
renameModule' fromName toName = transformBi (\n -> if n == fromName then toName else n)
