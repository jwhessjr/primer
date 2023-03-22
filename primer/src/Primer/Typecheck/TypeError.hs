module Primer.Typecheck.TypeError (TypeError (..)) where

import Foreword

import Primer.Core (Expr)
import Primer.Core.Meta (TmVarRef, TyConName, ValConName)
import Primer.Core.Type (Type')
import Primer.JSON (CustomJSON (..), FromJSON, PrimerJSON, ToJSON)
import Primer.Name (Name)
import Primer.Typecheck.KindError (KindError)

data TypeError
  = InternalError Text
  | UnknownVariable TmVarRef
  | TmVarWrongSort Name -- type var instead of term var
  | -- | Constructors (term-level) only inhabit fully-applied ADTs
    -- i.e. @Maybe a@, but not @Maybe@, @Maybe a b@, @Nat -> Bool@ or holes
    ConstructorNotFullAppADT (Type' ()) ValConName
  | -- | This ADT does not have a constructor of that name
    ConstructorWrongADT TyConName ValConName
  | -- | A constructor has inconsistently-kinded type arguments
    -- (wrt the ADT containing the constructor)
    ConstructorTypeArgsKinding
    -- TODO (saturated constructors) this is a temporary situation, and this
    -- error will be removed once constructors do not store their indices
  | -- | A constructor has the wrong number of type arguments
    -- (wrt the type we are checking it at)
    ConstructorTypeArgsInconsistentNumber
    -- TODO (saturated constructors) this is a temporary situation, and this
    -- error will be removed once constructors do not store their indices
  | -- | A constructor has the inconsistent type arguments
    -- (wrt the type we are checking it at)
    ConstructorTypeArgsInconsistentTypes
    -- TODO (saturated constructors) this is a temporary situation, and this
    -- error will be removed once constructors do not store their indices
  | UnknownConstructor ValConName
  | -- | Constructors (term-level) must be saturated.
    -- This error catches both under- and over-saturation.
    UnsaturatedConstructor ValConName
    -- TODO (saturated constructors) currently this catches both "wrong number
    -- of type/term arguments", but when constructors become checkable, then
    -- they will only have term arguments
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
  deriving stock (Eq, Show, Read, Generic)
  deriving (FromJSON, ToJSON) via PrimerJSON TypeError
  deriving anyclass (NFData)
