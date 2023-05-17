{-# LANGUAGE GADTs #-}

-- | Typechecking for Core expressions.
-- This closely follows the type system of Hazelnut, but supports arbitrary
-- types rather than just numbers.
-- In future we will want to extend it to support more features such as
-- polymorphism.
module Primer.Typecheck (
  Type,
  Expr,
  ExprT,
  synth,
  check,
  synthKind,
  checkKind,
  Cxt (..),
  KindOrType (..),
  initialCxt,
  buildTypingContext,
  TypeError (..),
  KindError (..),
  typeOf,
  matchArrowType,
  matchForallType,
  lookupGlobal,
  lookupVar,
  consistentKinds,
  consistentTypes,
  extendLocalCxtTy,
  extendLocalCxtTys,
  extendLocalCxt,
  extendGlobalCxt,
  extendTypeDefCxt,
  localTyVars,
) where

import Foreword

import Control.Monad.Fresh (MonadFresh (..))
import Control.Monad.NestedError (MonadNestedError (..), modifyError')
import Data.Map qualified as M
import Data.Map.Strict qualified as Map
import Optics (
  set,
 )
import Primer.Core (
  Expr,
  Expr' (..),
  GVarName,
  ID,
  Kind (..),
  LVarName,
  Meta (..),
  TmVarRef (..),
  TyVarName,
  Type' (..),
  TypeCache (..),
  TypeCacheBoth (..),
  unLocalName,
 )
import Primer.Core.Utils (
  alphaEqTy,
  forgetTypeMetadata,
  freshLocalName,
 )
import Primer.Def (
  DefMap,
  defType,
 )
import Primer.Name (NameCounter)
import Primer.Subst (substTy)
import Primer.Typecheck.Cxt (Cxt (Cxt, globalCxt, localCxt))
import Primer.Typecheck.Kindcheck (
  KindError (..),
  KindOrType (K, T),
  Type,
  TypeT,
  annotate,
  checkKind,
  consistentKinds,
  extendLocalCxtTy,
  extendLocalCxtTys,
  localTyVars,
  synthKind,
 )
import Primer.Typecheck.TypeError (TypeError (..))
import Primer.Typecheck.Utils (
  typeOf,
  _typecache,
 )

-- | Typechecking takes as input an Expr with 'Maybe Type' annotations and
-- produces an Expr with 'Type' annotations - i.e. every node in the output is
-- given a type. The type annotation isn't itself part of the editable program
-- so it has no metadata - hence the '()' argument inside 'TypeCache'.
--
-- The 'Type' annotations cache the type which a term synthesised/was checked
-- at. For "embeddings" where typechecking defers to synthesis, we record the
-- synthesised type, not the checked one. For example, when checking that
-- @Int -> ?@ accepts @\x . x@, we record that the variable node has type
-- @Int@, rather than @?@.
type ExprT = Expr' (Meta TypeCache) (Meta Kind)

lookupLocal :: LVarName -> Cxt -> Either TypeError Type
lookupLocal v cxt = case M.lookup (unLocalName v) $ localCxt cxt of
  Just (T t) -> Right t
  Just (K _) -> Left $ TmVarWrongSort (unLocalName v)
  Nothing -> Left $ UnknownVariable $ LocalVarRef v

lookupGlobal :: GVarName -> Cxt -> Maybe Type
lookupGlobal v cxt = M.lookup v $ globalCxt cxt

lookupVar :: TmVarRef -> Cxt -> Either TypeError Type
lookupVar v cxt = case v of
  LocalVarRef name -> lookupLocal name cxt
  GlobalVarRef name ->
    ( \case
        Just t -> Right t
        Nothing -> Left $ UnknownVariable v
    )
      (lookupGlobal name cxt)

extendLocalCxt :: (LVarName, Type) -> Cxt -> Cxt
extendLocalCxt (name, ty) cxt = cxt{localCxt = Map.insert (unLocalName name) (T ty) (localCxt cxt)}

extendGlobalCxt :: [(GVarName, Type)] -> Cxt -> Cxt
extendGlobalCxt globals cxt = cxt{globalCxt = Map.fromList globals <> globalCxt cxt}

extendTypeDefCxt :: x -> Cxt -> Cxt
extendTypeDefCxt _ cxt = cxt

-- An empty typing context
initialCxt :: Cxt
initialCxt =
  Cxt
    { localCxt = mempty
    , globalCxt = mempty
    }

-- | Construct an initial typing context, with all given definitions in scope as global variables.
buildTypingContext :: x -> DefMap -> Cxt
buildTypingContext tydefs defs =
  let globals = Map.assocs $ fmap defType defs
   in extendTypeDefCxt tydefs $ extendGlobalCxt globals initialCxt

-- | A shorthand for the constraints needed when kindchecking
type TypeM e m =
  ( Monad m
  , MonadReader Cxt m -- has access to a typing context, and SmartHoles option
  , MonadFresh ID m -- can generate fresh IDs
  -- can generate fresh names (needed for "smart holes" and polymorphism)
  , MonadFresh NameCounter m
  , MonadNestedError TypeError e m -- can throw type errors
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
synth :: TypeM e m => Expr -> m (Type, ExprT)
synth = \case
  Ann i e t -> do
    -- Check that the type is well-formed by synthesising its kind
    t' <- checkKind' KType t
    let t'' = forgetTypeMetadata t'
    -- Check e against the annotation
    e' <- check t'' e
    -- Annotate the Ann with the same type as e
    pure $ annSynth2 t'' i Ann e' t'
  EmptyHole i -> pure $ annSynth0 (TEmptyHole ()) i EmptyHole
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
  e -> throwError' $ CannotSynthesiseType e
  where
    -- We could combine these with some type class shenanigans, but it doesn't
    -- seem worth it. The general scheme is
    -- annSynthN t i c x1 ... xn = (t,c (annotate (TCSynthed t) i) x1 ... xn)
    annSynth0 t i x = (t, x $ annotate (TCSynthed t) i)
    annSynth1 t i c = annSynth0 t i . flip c
    annSynth2 t i c = annSynth1 t i . flip c
    annSynth3 t i c = annSynth2 t i . flip c
    annSynth4 t i c = annSynth3 t i . flip c

-- | Similar to synth, but for checking rather than synthesis.
check :: TypeM e m => Type -> Expr -> m ExprT
check t = \case
  Case i e brs -> do
    (eT, e') <- synth e
    let caseMeta = annotate (TCChkedAt t) i
    let isHoleTy = case eT of
            TEmptyHole{} -> True
            _ -> False
    if isHoleTy
     then if null brs
          then pure $ Case caseMeta e' []
          else throwError' CaseOfHoleNeedsEmptyBranches
     else throwError' $ CannotCaseNonADT eT
  e -> do
      (t', e') <- synth e
      if consistentTypes t t'
        then pure (set _typecache (TCEmb TCBoth{tcChkedAt = t, tcSynthed = t'}) e')
        else throwError' (InconsistentTypes t t')


-- | Checks if a type can be unified with a function (arrow) type. Returns the
-- arrowised version - i.e. if it's a hole then it returns an arrow type with
-- holes on both sides.
matchArrowType :: Type -> Maybe (Type, Type)
matchArrowType (TEmptyHole _) = pure (TEmptyHole (), TEmptyHole ())
matchArrowType (TFun _ a b) = pure (a, b)
matchArrowType _ = Nothing

-- | Checks if a type can be hole-refined to a forall, and if so returns the
-- forall'd version.
matchForallType :: MonadFresh NameCounter m => Type -> m (Maybe (TyVarName, Kind, Type))
-- These names will never enter the program, so we don't need to avoid shadowing
matchForallType (TEmptyHole _) = (\n -> Just (n, KHole, TEmptyHole ())) <$> freshLocalName mempty
matchForallType _ = pure Nothing

-- | Two types are consistent if they are equal (up to IDs and alpha) when we
-- also count holes as being equal to anything.
consistentTypes :: Type -> Type -> Bool
consistentTypes x y = uncurry eqType $ holepunch x y
  where
    -- We punch holes in each type so they "match" in the sense that
    -- they have holes in the same places. (At least, until we find
    -- obviously different constructors.)
    holepunch (TEmptyHole _) _ = (TEmptyHole (), TEmptyHole ())
    holepunch _ (TEmptyHole _) = (TEmptyHole (), TEmptyHole ())
    holepunch (TFun _ s t) (TFun _ s' t') =
      let (hs, hs') = holepunch s s'
          (ht, ht') = holepunch t t'
       in (TFun () hs ht, TFun () hs' ht')
    holepunch (TApp _ s t) (TApp _ s' t') =
      let (hs, hs') = holepunch s s'
          (ht, ht') = holepunch t t'
       in (TApp () hs ht, TApp () hs' ht')
    holepunch s t = (s, t)

-- | Compare two types for alpha equality, ignoring their IDs
eqType :: Type' a -> Type' b -> Bool
eqType t1 t2 = forgetTypeMetadata t1 `alphaEqTy` forgetTypeMetadata t2

checkKind' :: TypeM e m => Kind -> Type' (Meta a) -> m TypeT
checkKind' k t = modifyError' KindError (checkKind k t)
