module TypeError (TypeError (..)) where

import Foreword

import Core (Expr)
import KindError (KindError)
import Meta (TmVarRef, TyConName, ValConName)
import Name (Name)
import Type (Type')

data TypeError
  = InternalError Text
  | UnknownVariable TmVarRef
  | TmVarWrongSort Name -- type var instead of term var
  | UnknownConstructor ValConName
  | -- | Cannot use a PrimCon when either no type of the appropriate name is
    -- in scope, or it is a user-defined type
    PrimitiveTypeNotInScope TyConName
  | CannotSynthesiseType Expr
  | InconsistentTypes (Type' ()) (Type' ())
  | TypeDoesNotMatchArrow (Type' ())
  | TypeDoesNotMatchForall (Type' ())
  | CaseOfHoleNeedsEmptyBranches
  | CannotCaseNonADT (Type' ())
  | CannotCaseNonSaturatedADT (Type' ())
  | -- | Either wrong number, wrong constructors or wrong order. The fields are @name of the ADT@, @branches given@
    WrongCaseBranches TyConName [ValConName]
  | CaseBranchWrongNumberPatterns
  | KindError KindError
  deriving stock (Eq, Show, Read)
