-- | This module defines some builtin types that are used to seed initial programs.
--   The definitions here are no different than ones than a user can create, except
--   for the fact that some of the primitive functions (see "Primer.Primitives")
--   refer to these types.
module Primer.Builtins (
  tBool,
  tNat,
  tMaybe,
) where

import Primer.Core.Meta (
  GlobalName,
  ModuleName,
  TyConName,
  mkSimpleModuleName,
  qualifyName,
 )
import Primer.Name (Name)

builtinModuleName :: ModuleName
builtinModuleName = mkSimpleModuleName "Builtins"

builtin :: Name -> GlobalName k
builtin = qualifyName builtinModuleName

tBool :: TyConName
tBool = builtin "Bool"

tNat :: TyConName
tNat = builtin "Nat"

tMaybe :: TyConName
tMaybe = builtin "Maybe"
