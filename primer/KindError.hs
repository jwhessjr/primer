module KindError (KindError (..)) where

import Foreword

import Meta (TyConName, TyVarName)
import Type (Kind)
import Name (Name)

data KindError
  = UnknownTypeVariable TyVarName
  | TyVarWrongSort Name -- term var instead of type var
  | UnknownTypeConstructor TyConName
  | InconsistentKinds Kind Kind
  | KindDoesNotMatchArrow Kind
  | -- | We currently cannot typecheck a let inside a type,
    -- they should only transiently appear in evaluation, as explicit substitutions.
    TLetUnsupported
  deriving stock (Eq, Show, Read)
