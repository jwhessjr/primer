module Primer.Module (
  Module (..),
  qualifyTyConName,
  moduleTypesQualified,
  qualifyDefName,
  moduleDefsQualified,
) where

import Foreword

import Data.Data (Data)
import Data.Map (mapKeys)
import Primer.Core (
  GVarName,
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

data Module = Module
  { moduleName :: ModuleName
  , moduleTypes :: Map Name (TypeDef TypeMeta)
  , moduleDefs :: Map Name Def -- The current program: a set of definitions indexed by Name
  }
  deriving stock (Eq, Show, Read, Data)

qualifyTyConName :: Module -> Name -> TyConName
qualifyTyConName m = qualifyName (moduleName m)

moduleTypesQualified :: Module -> TypeDefMap
moduleTypesQualified m = mapKeys (qualifyTyConName m) $ forgetTypeDefMetadata <$> moduleTypes m

qualifyDefName :: Module -> Name -> GVarName
qualifyDefName m = qualifyName (moduleName m)

moduleDefsQualified :: Module -> DefMap
moduleDefsQualified m = mapKeys (qualifyDefName m) $ moduleDefs m
