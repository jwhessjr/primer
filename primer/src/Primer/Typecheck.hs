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
  SmartHoles (..),
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
  decomposeTAppCon,
  mkTAppCon,
  TypeDefInfo (..),
  TypeDefError (..),
  getTypeDefInfo',
  lookupConstructor,
  instantiateValCons,
  instantiateValCons',
  getGlobalNames,
  getGlobalBaseNames,
  lookupGlobal,
  lookupVar,
  consistentKinds,
  consistentTypes,
  extendLocalCxtTy,
  extendLocalCxtTys,
  extendLocalCxt,
  extendLocalCxts,
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
  over,
  set,
 )
import Primer.Core (
  CaseBranch' (..),
  Expr,
  Expr' (..),
  ExprMeta,
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
  TypeMeta,
  ValConName,
  bindName,
  unLocalName,
  _bindMeta,
 )
import Primer.Core.Transform (decomposeTAppCon, mkTAppCon)
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
import Primer.TypeDef (
  TypeDefMap,
  valConType,
 )
import Primer.Typecheck.Cxt (Cxt (Cxt, globalCxt, localCxt, smartHoles, typeDefs))
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
import Primer.Typecheck.SmartHoles (SmartHoles (..))
import Primer.Typecheck.TypeError (TypeError (..))
import Primer.Typecheck.Utils (
  TypeDefError (TDIHoleType, TDINotADT, TDINotSaturated, TDIUnknown),
  TypeDefInfo (TypeDefInfo),
  getGlobalBaseNames,
  getGlobalNames,
  getTypeDefInfo',
  instantiateValCons,
  instantiateValCons',
  lookupConstructor,
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

assert :: MonadNestedError TypeError e m => Bool -> Text -> m ()
assert b s = unless b $ throwError' (InternalError s)

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

extendLocalCxts :: [(LVarName, Type)] -> Cxt -> Cxt
extendLocalCxts x cxt = cxt{localCxt = Map.fromList (bimap unLocalName T <$> x) <> localCxt cxt}

extendGlobalCxt :: [(GVarName, Type)] -> Cxt -> Cxt
extendGlobalCxt globals cxt = cxt{globalCxt = Map.fromList globals <> globalCxt cxt}

extendTypeDefCxt :: TypeDefMap -> Cxt -> Cxt
extendTypeDefCxt typedefs cxt = cxt{typeDefs = typedefs <> typeDefs cxt}

-- An empty typing context
initialCxt :: SmartHoles -> Cxt
initialCxt sh =
  Cxt
    { smartHoles = sh
    , typeDefs = mempty
    , localCxt = mempty
    , globalCxt = mempty
    }

-- | Construct an initial typing context, with all given definitions in scope as global variables.
buildTypingContext :: TypeDefMap -> DefMap -> SmartHoles -> Cxt
buildTypingContext tydefs defs sh =
  let globals = Map.assocs $ fmap defType defs
   in extendTypeDefCxt tydefs $ extendGlobalCxt globals $ initialCxt sh

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
  Var i x -> do
    t <- either throwError' pure . lookupVar x =<< ask
    pure $ annSynth1 t i Var x
  App i e1 e2 -> do
    -- Synthesise e1
    (t1, e1') <- synth e1
    -- Check that e1 has an arrow type
    case matchArrowType t1 of
      Just (t2, t) -> do
        -- Check e2 against the domain type of e1
        e2' <- check t2 e2
        pure $ annSynth2 t i App e1' e2'
      Nothing ->
        asks smartHoles >>= \case
          NoSmartHoles -> throwError' $ TypeDoesNotMatchArrow t1
  APP i e t -> do
    (et, e') <- synth e
    matchForallType et >>= \case
      Just (v, vk, b) -> do
        t' <- checkKind' vk t
        bSub <- substTy v (forgetTypeMetadata t') b
        pure (bSub, APP (annotate (TCSynthed bSub) i) e' t')
      Nothing ->
        asks smartHoles >>= \case
          NoSmartHoles -> throwError' $ TypeDoesNotMatchForall et
  Ann i e t -> do
    -- Check that the type is well-formed by synthesising its kind
    t' <- checkKind' KType t
    let t'' = forgetTypeMetadata t'
    -- Check e against the annotation
    e' <- check t'' e
    -- Annotate the Ann with the same type as e
    pure $ annSynth2 t'' i Ann e' t'
  EmptyHole i -> pure $ annSynth0 (TEmptyHole ()) i EmptyHole
  -- We assume that constructor names are unique
  -- See Note [Synthesisable constructors] in Core.hs
  Con i c -> do
    asks (flip lookupConstructor c . typeDefs) >>= \case
      Just (vc, tc, td) -> let t = valConType tc td vc in pure $ annSynth1 t i Con c
      Nothing -> throwError' $ UnknownConstructor c
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
  Hole i e -> do
    (_, e') <- synth e
    pure $ annSynth1 (TEmptyHole ()) i Hole e'
  Let i x a b -> do
    -- Synthesise a type for the bound expression
    (aT, a') <- synth a
    -- Extend the context with the binding, and synthesise the body
    (bT, b') <- local (extendLocalCxt (x, aT)) $ synth b
    pure $ annSynth3 bT i Let x a' b'
  Letrec i x a tA b -> do
    -- Check that tA is well-formed
    tA' <- checkKind' KType tA
    let t = forgetTypeMetadata tA'
        ctx' = extendLocalCxt (x, t)
    -- Check the bound expression against its annotation
    a' <- local ctx' $ check t a
    -- Extend the context with the binding, and synthesise the body
    (bT, b') <- local ctx' $ synth b
    pure $ annSynth4 bT i Letrec x a' tA' b'
  e ->
    asks smartHoles >>= \case
      NoSmartHoles -> throwError' $ CannotSynthesiseType e
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
  Lam i x e -> do
    case matchArrowType t of
      Just (t1, t2) -> do
        e' <- local (extendLocalCxt (x, t1)) $ check t2 e
        pure (Lam (annotate (TCChkedAt t) i) x e')
      Nothing ->
        asks smartHoles >>= \case
          NoSmartHoles -> throwError' $ TypeDoesNotMatchArrow t
  LAM i n e -> do
    matchForallType t >>= \case
      Just (m, k, b) -> do
        b' <- substTy m (TVar () n) b
        e' <- local (extendLocalCxtTy (n, k)) $ check b' e
        pure $ LAM (annotate (TCChkedAt t) i) n e'
      Nothing ->
        asks smartHoles >>= \case
          NoSmartHoles -> throwError' $ TypeDoesNotMatchForall t
  Let i x a b -> do
    -- Synthesise a type for the bound expression
    (aT, a') <- synth a
    -- Extend the context with the binding, and check the body against the type
    b' <- local (extendLocalCxt (x, aT)) $ check t b
    -- NB here: if b were synthesisable, we bubble that information up to the
    -- let, saying @typeOf b'@ rather than @TCChkedAt t@
    -- TODO: why do we do this?
    pure $ Let (annotate (typeOf b') i) x a' b'
  Letrec i x a tA b -> do
    -- Check that tA is well-formed
    tA' <- checkKind' KType tA
    let ctx' = extendLocalCxt (x, forgetTypeMetadata tA')
    -- Check the bound expression against its annotation
    a' <- local ctx' $ check (forgetTypeMetadata tA') a
    -- Extend the context with the binding, and synthesise the body
    b' <- local ctx' $ check t b
    pure $ Letrec (annotate (TCChkedAt t) i) x a' tA' b'
  Case i e brs -> do
    (eT, e') <- synth e
    let caseMeta = annotate (TCChkedAt t) i
    instantiateValCons eT >>= \case
      -- we allow 'case' on a thing of type TEmptyHole iff we have zero branches
      Left TDIHoleType ->
        if null brs
          then pure $ Case caseMeta e' []
          else
            asks smartHoles >>= \case
              NoSmartHoles -> throwError' CaseOfHoleNeedsEmptyBranches
      Left TDINotADT ->
        asks smartHoles >>= \case
          NoSmartHoles -> throwError' $ CannotCaseNonADT eT
      Left (TDIUnknown ty) -> throwError' $ InternalError $ "We somehow synthesised the unknown type " <> show ty <> " for the scrutinee of a case"
      Left TDINotSaturated ->
        asks smartHoles >>= \case
          NoSmartHoles -> throwError' $ CannotCaseNonSaturatedADT eT
      Right (tc, _, expected) -> do
        let branchNames = map (\(CaseBranch n _ _) -> n) brs
        let conNames = map fst expected
        sh <- asks smartHoles
        brs' <- case (branchNames == conNames, sh) of
          (False, NoSmartHoles) -> throwError' $ WrongCaseBranches tc branchNames
          -- create branches with the correct name but wrong parameters,
          -- they will be fixed up in checkBranch later
          (True, _) -> pure brs
        brs'' <- zipWithM (checkBranch t) expected brs'
        pure $ Case caseMeta e' brs''
  e -> do
    sh <- asks smartHoles
    let default_ = do
          (t', e') <- synth e
          if consistentTypes t t'
            then pure (set _typecache (TCEmb TCBoth{tcChkedAt = t, tcSynthed = t'}) e')
            else case sh of
              NoSmartHoles -> throwError' (InconsistentTypes t t')
    case (e, sh) of
      -- If the hole can be dropped leaving a type-correct term, do so
      -- We don't want the recursive call to create a fresh hole though -
      -- this can lead to the output being the same as the input, but with
      -- ID of the top hole changed, leading to losing cursor positions etc.
      -- But we do want to remove nested holes.
      _ -> default_

-- | Similar to check, but for the RHS of case branches
-- We assume that the branch is for this constructor
-- The passed in 'ValCon' is assumed to be a constructor of the passed in 'TypeDef'
checkBranch ::
  forall e m.
  TypeM e m =>
  Type ->
  (ValConName, [Type' ()]) -> -- The constructor and its instantiated parameter types
  CaseBranch' ExprMeta TypeMeta ->
  m (CaseBranch' (Meta TypeCache) (Meta Kind))
checkBranch t (vc, args) (CaseBranch nb patterns rhs) =
  do
    -- We check an invariant due to paranoia
    assertCorrectCon
    sh <- asks smartHoles
    (fixedPats, fixedRHS) <- case (length args == length patterns, sh) of
      (False, NoSmartHoles) -> throwError' CaseBranchWrongNumberPatterns
      -- if the branch is nonsense, replace it with a sensible pattern and an empty hole
      (True, _) ->
        let args' = zipWith (\ty bind -> (over _bindMeta (annotate (TCChkedAt ty)) bind, ty)) args patterns
         in pure (args', rhs)
    rhs' <- local (extendLocalCxts (map (first bindName) fixedPats)) $ check t fixedRHS
    pure $ CaseBranch nb (map fst fixedPats) rhs'
  where
    assertCorrectCon =
      assert (vc == nb) $
        "checkBranch: expected a branch on "
          <> show vc
          <> " but found branch on "
          <> show nb

-- | Checks if a type can be unified with a function (arrow) type. Returns the
-- arrowised version - i.e. if it's a hole then it returns an arrow type with
-- holes on both sides.
matchArrowType :: Type -> Maybe (Type, Type)
matchArrowType (TEmptyHole _) = pure (TEmptyHole (), TEmptyHole ())
matchArrowType (THole _ _) = pure (TEmptyHole (), TEmptyHole ())
matchArrowType (TFun _ a b) = pure (a, b)
matchArrowType _ = Nothing

-- | Checks if a type can be hole-refined to a forall, and if so returns the
-- forall'd version.
matchForallType :: MonadFresh NameCounter m => Type -> m (Maybe (TyVarName, Kind, Type))
-- These names will never enter the program, so we don't need to avoid shadowing
matchForallType (TEmptyHole _) = (\n -> Just (n, KHole, TEmptyHole ())) <$> freshLocalName mempty
matchForallType (THole _ _) = (\n -> Just (n, KHole, TEmptyHole ())) <$> freshLocalName mempty
matchForallType (TForall _ a k t) = pure $ Just (a, k, t)
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
    holepunch (THole _ _) _ = (TEmptyHole (), TEmptyHole ())
    holepunch _ (THole _ _) = (TEmptyHole (), TEmptyHole ())
    holepunch (TFun _ s t) (TFun _ s' t') =
      let (hs, hs') = holepunch s s'
          (ht, ht') = holepunch t t'
       in (TFun () hs ht, TFun () hs' ht')
    holepunch (TApp _ s t) (TApp _ s' t') =
      let (hs, hs') = holepunch s s'
          (ht, ht') = holepunch t t'
       in (TApp () hs ht, TApp () hs' ht')
    holepunch (TForall _ n k s) (TForall _ m l t) =
      let (hs, ht) = holepunch s t
       in -- Perhaps we need to compare the kinds up to holes also?
          (TForall () n k hs, TForall () m l ht)
    holepunch s t = (s, t)

-- | Compare two types for alpha equality, ignoring their IDs
eqType :: Type' a -> Type' b -> Bool
eqType t1 t2 = forgetTypeMetadata t1 `alphaEqTy` forgetTypeMetadata t2

checkKind' :: TypeM e m => Kind -> Type' (Meta a) -> m TypeT
checkKind' k t = modifyError' KindError (checkKind k t)
