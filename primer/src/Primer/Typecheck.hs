{-# LANGUAGE GADTs #-}

-- | Typechecking for Core expressions.
-- This closely follows the type system of Hazelnut, but supports arbitrary
-- types rather than just numbers.
-- In future we will want to extend it to support more features such as
-- polymorphism.
module Primer.Typecheck (
  Expr (..),
  Kind (..),
  Type (..),
  synth,
  check,
  synthKind,
  checkKind,
  TypeError (..),
  KindError (..),
  consistentKinds,
  consistentTypes,
  refine,
) where

import Prelude

import Control.Monad.Except (MonadError (throwError))
import Control.Monad.Trans.Except (ExceptT, runExceptT)

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

-- | A shorthand for the constraints needed when kindchecking
type KindM m =
  ( Monad m
  , MonadError KindError m -- can throw kind errors
  )

-- Synthesise a kind for the given type
-- TypeHoles are always considered to have kind KHole - a kind hole.
-- When SmartHoles is on, we essentially remove all holes, and re-insert where
-- necessary.
-- However, we take care not to remove a non-empty hole only to immediately
-- re-insert it, since this would needlessly change its ID, resulting in
-- problems if an action left the cursor on such a hole: "lost ID after
-- typechecking". For example, consider (numbers are denoting IDs inside the
-- metadata)
--   synthKind $ TApp 0 (THole 1 (TCon 2 Bool)) t
-- If we removed the hole, we would then note that Bool does not have an arrow
-- kind, and so wrap it in a hole again, returning something like
--   TApp 0 (THole 3 (TCon 2 Bool)) t
-- A similar thing would happen with
--   synthKind $ TApp 0 (TCon 1 List) (THole 2 (TCon 3 List))
-- because we do not have checkKind KType List
synthKind :: KindM m => Type -> m (Kind, Type)
synthKind = \case
  TEmptyHole -> pure (KHole, TEmptyHole)
  TFun a b -> do
    a' <- checkKind KType a
    b' <- checkKind KType b
    pure (KType, TFun a' b')
  TApp s t -> do
    (k, s') <- synthKind s
    case matchArrowKind k of
      Nothing -> throwError $ KindDoesNotMatchArrow k
      Just (k1, k2) -> checkKind k1 t >>= \t' -> pure (k2, TApp s' t')

checkKind :: KindM m => Kind -> Type -> m Type
checkKind k t = do
  (k', t') <- synthKind t
  if consistentKinds k k'
    then pure t'
    else throwError $ InconsistentKinds k k'

matchArrowKind :: Kind -> Maybe (Kind, Kind)
matchArrowKind KHole = pure (KHole, KHole)
matchArrowKind KType = Nothing
matchArrowKind (KFun k1 k2) = pure (k1, k2)

consistentKinds :: Kind -> Kind -> Bool
consistentKinds KHole _ = True
consistentKinds _ KHole = True
consistentKinds KType KType = True
consistentKinds (KFun k1 k2) (KFun k1' k2') = consistentKinds k1 k1' && consistentKinds k2 k2'
consistentKinds _ _ = False


data TypeError
  = CannotSynthesiseType Expr
  | InconsistentTypes Type Type
  | CaseOfHoleNeedsEmptyBranches
  | CannotCaseNonADT Type
  | KindError KindError
  deriving stock (Eq, Show)

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
consistentTypes TEmptyHole _ = True
consistentTypes _ TEmptyHole = True
consistentTypes (TFun s1 t1) (TFun s2 t2) = consistentTypes s1 s2 && consistentTypes t1 t2
consistentTypes (TApp s1 t1) (TApp s2 t2) = consistentTypes s1 s2 && consistentTypes t1 t2
consistentTypes _ _ = False

checkKind' :: TypeM m => Kind -> Type -> m Type
checkKind' k t = modifyError KindError (checkKind k t)

modifyError :: MonadError e' m => (e -> e') -> ExceptT e m a -> m a
modifyError f m = runExceptT m >>= either (throwError . f) pure


refine :: Type -> Type -> Maybe Type
refine tgtTy tmTy = if consistentTypes tgtTy tmTy
          then Just tmTy
          else case tmTy of
                 TFun _ t -> refine tgtTy t
                 _ -> Nothing
