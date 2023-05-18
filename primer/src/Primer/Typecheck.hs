module Primer.Typecheck (
  Expr (..),
  Kind (..),
  Type (..),
  TypeError (..),
  KindError (..),
  consistentTypes,
  refine,
) where

import Prelude

data Type
  = TEmptyHole
  | TFun Type Type
  | TApp Type Type
  deriving stock (Eq, Show)

-- | Core kinds.
data Kind = KHole | KType | KFun Kind Kind
  deriving stock (Eq, Show)

data Expr
  = EmptyHole
  | Ann Expr Type
  | Case Expr [CaseBranch]
  deriving stock (Eq, Show)

data CaseBranch = CaseBranch
  deriving stock (Eq, Show)

data KindError
  = InconsistentKinds Kind Kind
  | KindDoesNotMatchArrow Kind
  deriving stock (Eq, Show)


data TypeError
  = CannotSynthesiseType Expr
  | InconsistentTypes Type Type
  | CaseOfHoleNeedsEmptyBranches
  | CannotCaseNonADT Type
  | KindError KindError
  deriving stock (Eq, Show)

consistentTypes :: Type -> Type -> Bool
consistentTypes TEmptyHole _ = True
consistentTypes _ TEmptyHole = True
consistentTypes (TFun s1 t1) (TFun s2 t2) = consistentTypes s1 s2 && consistentTypes t1 t2
consistentTypes (TApp s1 t1) (TApp s2 t2) = consistentTypes s1 s2 && consistentTypes t1 t2
consistentTypes _ _ = False


refine :: Type -> Type -> Maybe Type
refine tgtTy tmTy = if consistentTypes tgtTy tmTy
          then Just tmTy
          else case tmTy of
                 TFun _ t -> refine tgtTy t
                 _ -> Nothing
