{-# LANGUAGE GADTs #-}

-- | Typechecking for Core expressions.
-- This closely follows the type system of Hazelnut, but supports arbitrary
-- types rather than just numbers.
-- In future we will want to extend it to support more features such as
-- polymorphism.
module Primer.Typecheck (
  Type,
  Expr,
  synth,
  check,
  synthKind,
  checkKind,
  TypeError (..),
  KindError (..),
  consistentKinds,
  consistentTypes,
) where

import Foreword

import Primer.Core (
  Expr (..),
 )
import Primer.Core.Type (
  Kind (..),
  Type (..),
 )
import Primer.Typecheck.Kindcheck (
  KindError (..),
  checkKind,
  consistentKinds,
  synthKind,
 )


data TypeError
  = InternalError Text
  | CannotSynthesiseType Expr
  | InconsistentTypes Type Type
  | CaseOfHoleNeedsEmptyBranches
  | CannotCaseNonADT Type
  | KindError KindError
  deriving stock (Eq, Show, Read)

-- | A shorthand for the constraints needed when kindchecking
type TypeM m =
  ( Monad m
  , MonadError TypeError m -- can throw type errors
  )

{- HLINT ignore synth "Avoid lambda using `infix`" -}
-- Note [Let expressions]
-- Let expressions are typechecked flexibly in order to minimise the instances
-- where an annotation must be added. Hence we can both synthesise and check
-- let expressions.
--
-- We can currently use lets to mimic top-level definitions, but when top-level
-- definitions become a first-class concept will we want to enforce that they
-- have an explicit type declaration.

-- | Synthesise a type for an expression.
-- We optionally insert/remove holes and insert annotations where
-- needed/possible, based on the SmartHoles option in the TypeM reader monad.
-- When we 'NoSmartHoles', the output will be the same as the input, modulo
-- caching type information in the metadata.
-- When we insert and remove holes and annotations, the AST may change.
-- The only changes we make to the AST are
-- - wrapping/unwrapping holes and annotations to make the types line up.
-- - recreating case branches if necessary (deleting their RHSs)
-- We return the synthesised type so one does not need to rely on
-- the cached type in the output being TCSynthed.
-- INVARIANT: if @synth e@ gives @(T,e')@, then @e@ and @e'@ agree up to their
-- cached types, and @TCSynthed T == typeOf e'@
synth :: TypeM m => Expr -> m (Type, Expr)
synth = \case
  Ann e t -> do
    -- Check that the type is well-formed by synthesising its kind
    t' <- checkKind' KType t
    -- Check e against the annotation
    e' <- check t' e
    -- Annotate the Ann with the same type as e
    pure (t', Ann e' t')
  EmptyHole -> pure (TEmptyHole, EmptyHole)
  -- When synthesising a hole, we first check that the expression inside it
  -- synthesises a type successfully (see Note [Holes and bidirectionality]).
  -- TODO: we would like to remove this hole (leaving e) if possible, but I
  -- don't see how to do this nicely as we don't know what constraints the
  -- synthesised type needs. Consider {? 1 ?} True: we can't remove the hole,
  -- but we don't know that when we come to synthesise its type. Potentially we
  -- could remove it here and let the App rule re-add it if necessary, but then
  -- consider {? ? : Nat -> Nat ?} True: then we could remove the hole, and App
  -- would see the function has an arrow type and check Nat ∋ True which fails,
  -- leaving (? : Nat -> Nat) {? True ?}. This causes holes to jump around
  -- which is bad UX.
  -- See https://github.com/hackworthltd/primer/issues/7
  e -> throwError $ CannotSynthesiseType e

-- | Similar to synth, but for checking rather than synthesis.
check :: TypeM m => Type -> Expr -> m Expr
check t = \case
  Case e brs -> do
    (eT, e') <- synth e
    let isHoleTy = case eT of
            TEmptyHole{} -> True
            _ -> False
    if isHoleTy
     then if null brs
          then pure $ Case e' []
          else throwError CaseOfHoleNeedsEmptyBranches
     else throwError $ CannotCaseNonADT eT
  e -> do
      (t', e') <- synth e
      if consistentTypes t t'
        then pure e'
        else throwError (InconsistentTypes t t')

-- | Two types are consistent if they are equal (up to IDs and alpha) when we
-- also count holes as being equal to anything.
consistentTypes :: Type -> Type -> Bool
consistentTypes x y = uncurry (==) $ holepunch x y
  where
    -- We punch holes in each type so they "match" in the sense that
    -- they have holes in the same places. (At least, until we find
    -- obviously different constructors.)
    holepunch TEmptyHole _ = (TEmptyHole, TEmptyHole)
    holepunch _ TEmptyHole = (TEmptyHole, TEmptyHole)
    holepunch (TFun s t) (TFun s' t') =
      let (hs, hs') = holepunch s s'
          (ht, ht') = holepunch t t'
       in (TFun hs ht, TFun hs' ht')
    holepunch (TApp s t) (TApp s' t') =
      let (hs, hs') = holepunch s s'
          (ht, ht') = holepunch t t'
       in (TApp hs ht, TApp hs' ht')
    holepunch s t = (s, t)

checkKind' :: TypeM m => Kind -> Type -> m Type
checkKind' k t = modifyError KindError (checkKind k t)

modifyError :: MonadError e' m => (e -> e') -> ExceptT e m a -> m a
modifyError f m = runExceptT m >>= either (throwError . f) pure
