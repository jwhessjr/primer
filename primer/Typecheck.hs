{-# LANGUAGE ImpredicativeTypes #-}

-- | Typechecking for Core expressions.
-- This closely follows the type system of Hazelnut, but supports arbitrary
-- types rather than just numbers.
-- In future we will want to extend it to support more features such as
-- polymorphism.
module Typecheck (
  Type,
  Expr,
  ExprT,
  SmartHoles (..),
  synth,
  check,
  synthKind,
  checkKind,
  checkTypeDefs,
  CheckEverythingRequest (..),
  checkEverything,
  Cxt (..),
  KindOrType (..),
  initialCxt,
  buildTypingContext,
  buildTypingContextFromModules,
  buildTypingContextFromModules',
  TypeError (..),
  KindError (..),
  TypeDefInfo (..),
  TypeDefError (..),
  getTypeDefInfo',
  instantiateValCons,
  instantiateValCons',
  exprTtoExpr,
  typeTtoType,
  getGlobalNames,
  getGlobalBaseNames,
  consistentTypes,
  extendLocalCxt,
  extendLocalCxts,
  extendGlobalCxt,
  extendTypeDefCxt,
) where

import Foreword

import Control.Arrow ((&&&))
import Fresh (MonadFresh (..))
import NestedError (MonadNestedError (..), modifyError')
import Data.Functor.Compose (Compose (Compose, getCompose))
import Data.Foldable (foldMap')
import Data.Map qualified as M
import Data.Map.Strict qualified as Map
import Data.Set qualified as S
import Optics (
  A_Traversal,
  AppendIndices,
  IxFold,
  IxTraversal',
  JoinKinds,
  Lens',
  Optic',
  WithIx,
  equality,
  icompose,
  itoListOf,
  itraversed,
  over,
  reindexed,
  selfIndex,
  set,
  to,
  traverseOf,
  (%),
 )
import Optics.Traversal (traversed)
import Core (
  Bind' (..),
  CaseBranch' (..),
  Expr,
  Expr' (..),
  ExprMeta,
  GlobalName (baseName, qualifiedModule),
  GVarName,
  ID,
  Kind (..),
  LVarName,
  ModuleName,
  Meta (..),
  Type' (..),
  TypeCache (..),
  TypeCacheBoth (..),
  TypeMeta,
  ValConName,
  bindName,
  qualifyName,
  unLocalName,
  _bindMeta,
  _exprMeta,
  _exprMetaLens,
  _exprTypeMeta,
  _typeMeta,
 )
import DSL (S, branch, create', emptyHole, meta, meta')
import Meta (TyConName, _type)
import CoreUtils (
  alphaEqTy,
  forgetTypeMetadata,
  freshLocalName',
  noHoles,
 )
import Def (
  ASTDef (..),
  _astDefType,
  Def (..),
  _DefAST,
  DefMap,
  defType,
 )
import Module (
  Module (
    moduleName
  ),
  _moduleDefs,
  moduleDefsQualified,
  moduleTypesQualified,
 )
import Name (Name, NameCounter)
import TypeDef (
  ASTTypeDef,
  TypeDef (TypeDefAST),
  TypeDefMap,
  typeDefKind
 )
import TypeError (TypeError (..))
import KindError (
  KindError (
    InconsistentKinds,
    KindDoesNotMatchArrow,
    TLetUnsupported,
    TyVarWrongSort,
    UnknownTypeConstructor,
    UnknownTypeVariable
  ),
 )

type Type = Type' ()

data SmartHoles = SmartHoles | NoSmartHoles
  deriving stock (Eq, Show, Read)

data KindOrType = K Kind | T Type
  deriving stock (Show, Eq)

data Cxt = Cxt
  { smartHoles :: SmartHoles
  , typeDefs :: TypeDefMap
  , localCxt :: Map Name KindOrType
  -- ^ local variables. invariant: the Name comes from a @LocalName k@, and
  -- the tag @k@ should say whether the value is a kind or a type.
  -- We detect violations of this in 'lookupLocal' (thus we key this map
  -- by the underlying 'Name', rather than use a dependent map)
  , globalCxt :: Map GVarName Type
  -- ^ global variables (i.e. IDs of top-level definitions)
  }
  deriving stock (Show)

-- | A shorthand for the constraints needed when kindchecking
type KindM e m =
  ( Monad m
  , MonadReader Cxt m -- has access to a typing context, and SmartHoles option
  , MonadFresh ID m -- can generate fresh IDs
  -- can generate fresh names (needed for "smart holes" and polymorphism)
  , MonadFresh NameCounter m
  , MonadNestedError KindError e m -- can throw kind errors
  )

type TypeT = Type' (Meta Kind)

-- Synthesise a kind for the given type
synthKind :: KindM e m => Type' (Meta a) -> m (Kind, TypeT)
synthKind = \case
  TEmptyHole m -> pure (KType, TEmptyHole (annotate KType m))
  TCon m c -> do
    typeDef <- asks (Map.lookup c . typeDefs)
    case typeDef of
      Nothing -> throwError' $ UnknownTypeConstructor c
      Just def -> let k = typeDefKind def in pure (k, TCon (annotate k m) c)
  TFun m a b -> do
    a' <- checkKind KType a
    b' <- checkKind KType b
    pure (KType, TFun (annotate KType m) a' b')

checkKind :: KindM e m => Kind -> Type' (Meta a) -> m TypeT
checkKind _ t = do
  (_, t') <- synthKind t
  pure t'

-- | Extend the metadata of an 'Expr' or 'Type'
-- (usually with a 'TypeCache' or 'Kind')
annotate :: b -> Meta a -> Meta b
annotate t (Meta i _ v) = Meta i t v

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

buildTypingContextFromModules :: [Module] -> SmartHoles -> Cxt
buildTypingContextFromModules modules =
  buildTypingContext
    (foldMap' moduleTypesQualified modules)
    (foldMap' moduleDefsQualified modules)

buildTypingContextFromModules' :: [S Module] -> SmartHoles -> Cxt
-- NB: we don't care about IDs/TypeMeta here, since we remove them in
-- buildTypingContextFromModules, thus @create'@ is ok.
buildTypingContextFromModules' = buildTypingContextFromModules . create' . sequence

-- | A shorthand for the constraints needed when kindchecking
type TypeM e m =
  ( Monad m
  , MonadReader Cxt m -- has access to a typing context, and SmartHoles option
  , MonadFresh ID m -- can generate fresh IDs
  -- can generate fresh names (needed for "smart holes" and polymorphism)
  , MonadFresh NameCounter m
  , MonadNestedError TypeError e m -- can throw type errors
  )

-- | Check all type definitions, as one recursive group, in some monadic environment
checkTypeDefs ::
  TypeM e m =>
  TypeDefMap ->
  m ()
checkTypeDefs tds = do
  existingTypes <- asks typeDefs
  -- NB: we expect the frontend to only submit acceptable typedefs, so all
  -- errors here are "internal errors" and should never be seen.
  -- (This is not quite true, see
  -- https://github.com/hackworthltd/primer/issues/3)
  assert (Map.disjoint existingTypes tds) "Duplicate-ly-named TypeDefs"

data CheckEverythingRequest = CheckEverything
  { trusted :: [Module]
  , toCheck :: [Module]
  }

-- | Check a (mutually-recursive set of) module(s), in a given trusted
-- environment of modules.
-- Returns just the modules that were requested 'toCheck', with updated cached
-- type information etc.
--
-- This checks every type definition and every global term definition.
--
-- In particular, this typechecks all the definitions, in one recursive group.
-- This checks that the type signature is well-formed, then checks the body
-- (for an ASTDef) against the signature.
-- (If SmartHoles edits a type, the body of every function is checked in the
-- environment with the updated type)
checkEverything ::
  forall e m.
  (MonadFresh ID m, MonadFresh NameCounter m, MonadNestedError TypeError e (ReaderT Cxt m)) =>
  SmartHoles ->
  CheckEverythingRequest ->
  m [Module]
checkEverything sh CheckEverything{trusted, toCheck} =
  let cxt = buildTypingContextFromModules trusted sh
   in flip runReaderT cxt $ do
        let newTypes = foldMap' moduleTypesQualified toCheck
        checkTypeDefs newTypes
        local (extendTypeDefCxt newTypes) $ do
          -- Kind check and update (for smartholes) all the types.
          -- Note that this may give ill-typed definitions if the type changes
          -- since we have not checked the expressions against the new types.
          updatedTypes <- traverseOf (traverseDefs % _DefAST % _astDefType) (fmap typeTtoType . checkKind' KType) toCheck
          -- Now extend the context with the new types
          let defsUpdatedTypes = itoListOf foldDefTypesWithName updatedTypes
          local (extendGlobalCxt defsUpdatedTypes) $
            -- Check the body (of AST definitions) against the new type
            traverseOf
              (traverseDefs % _DefAST)
              ( \def -> do
                  e <- check (forgetTypeMetadata $ astDefType def) (astDefExpr def)
                  pure $ def{astDefExpr = exprTtoExpr e}
              )
              updatedTypes
  where
    -- The first argument of traverseDefs' is intended to either
    -- - be equality, giving a traveral
    -- - specify an index (using selfIndex and reindexed), giving a fold
    traverseDefs' ::
      ( JoinKinds A_Traversal k l
      , JoinKinds l A_Traversal l
      , AppendIndices is (WithIx Name) js
      ) =>
      Optic' k is Module Module ->
      Optic' l js [Module] Def
    traverseDefs' o = traversed % o % (_moduleDefs % itraversed)
    traverseDefs :: IxTraversal' Name [Module] Def
    traverseDefs = traverseDefs' equality
    foldDefTypesWithName :: IxFold GVarName [Module] Type
    foldDefTypesWithName =
      icompose qualifyName $
        traverseDefs' (reindexed moduleName selfIndex)
          % to defType
          % to forgetTypeMetadata

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
  -- We assume that constructor names are unique
  -- See Note [Synthesisable constructors] in Core.hs
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
  e ->
    asks smartHoles >>= \case
      NoSmartHoles -> throwError' $ CannotSynthesiseType e
      SmartHoles -> do
        ann <- TEmptyHole <$> meta' (Just KType)
        eMeta <- meta
        synth $ Ann eMeta e ann
  where
    -- We could combine these with some type class shenanigans, but it doesn't
    -- seem worth it. The general scheme is
    -- annSynthN t i c x1 ... xn = (t,c (annotate (TCSynthed t) i) x1 ... xn)
    annSynth0 t i x = (t, x $ annotate (TCSynthed t) i)
    annSynth1 t i c = annSynth0 t i . flip c
    annSynth2 t i c = annSynth1 t i . flip c

-- | Similar to synth, but for checking rather than synthesis.
check :: TypeM e m => Type -> Expr -> m ExprT
check t = \case
  lam@(Lam i x e) -> do
    case matchArrowType t of
      Just (t1, t2) -> do
        e' <- local (extendLocalCxt (x, t1)) $ check t2 e
        pure (Lam (annotate (TCChkedAt t) i) x e')
      Nothing ->
        asks smartHoles >>= \case
          NoSmartHoles -> throwError' $ TypeDoesNotMatchArrow t
          SmartHoles -> do
            -- 'synth' will take care of adding an annotation - no need to do it
            -- explicitly here
            (_, lam') <- synth lam
            Hole <$> meta' (TCEmb TCBoth{tcChkedAt = t, tcSynthed = TEmptyHole ()}) <*> pure lam'
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
              SmartHoles -> pure $ Case caseMeta e' []
      Left TDINotADT ->
        asks smartHoles >>= \case
          NoSmartHoles -> throwError' $ CannotCaseNonADT eT
          SmartHoles -> do
            -- NB: we wrap the scrutinee in a hole and DELETE the branches
            scrutWrap <- Hole <$> meta' (TCSynthed (TEmptyHole ())) <*> pure e'
            pure $ Case caseMeta scrutWrap []
      Left (TDIUnknown ty) -> throwError' $ InternalError $ "We somehow synthesised the unknown type " <> show ty <> " for the scrutinee of a case"
      Left TDINotSaturated ->
        asks smartHoles >>= \case
          NoSmartHoles -> throwError' $ CannotCaseNonSaturatedADT eT
          SmartHoles -> do
            -- NB: we wrap the scrutinee in a hole and DELETE the branches
            scrutWrap <- Hole <$> meta' (TCSynthed (TEmptyHole ())) <*> pure e'
            pure $ Case caseMeta scrutWrap []
      Right (tc, _, expected) -> do
        let branchNames = map (\(CaseBranch n _ _) -> n) brs
        let conNames = map fst expected
        sh <- asks smartHoles
        brs' <- case (branchNames == conNames, sh) of
          (False, NoSmartHoles) -> throwError' $ WrongCaseBranches tc branchNames
          -- create branches with the correct name but wrong parameters,
          -- they will be fixed up in checkBranch later
          (False, SmartHoles) -> traverse (\c -> branch c [] emptyHole) conNames
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
              SmartHoles -> Hole <$> meta' (TCEmb TCBoth{tcChkedAt = t, tcSynthed = TEmptyHole ()}) <*> pure e'
    case (e, sh) of
      -- If the hole can be dropped leaving a type-correct term, do so
      -- We don't want the recursive call to create a fresh hole though -
      -- this can lead to the output being the same as the input, but with
      -- ID of the top hole changed, leading to losing cursor positions etc.
      -- But we do want to remove nested holes.
      (Hole _ e'@Hole{}, SmartHoles) ->
        check t e' -- we strip off one layer, and hit this case again.
      (Hole _ (Ann _ e' TEmptyHole{}), SmartHoles) ->
        -- We do want to remove (e.g.) {? λx.x : ? ?} to get λx.x,
        -- if that typechecks. (But only a simple hole annotation, as we do
        -- not wish to delete any interesting annotations.)
        flip catchError (const default_) $
          check t e' >>= \case
            Hole{} -> default_ -- Don't let the recursive call mint a hole.
            e'' -> pure e''
      (Hole _ (Ann _ _ ty), SmartHoles)
        | not (noHoles ty) ->
            -- Don't want to, e.g., remove {? λx.x : ? ?} to get λx.x : ?
            -- Since holey annotations behave like non-empty holes, we will
            -- not elide non-empty holes if they have a holey annotation.
            -- (This is needed for idempotency, since we return non-empty
            -- holes with holey-annotated contents in the case a construction
            -- cannot typecheck, e.g. Bool ∋ λx.t returns {? λx.t : ? ?}
            default_
      (Hole _ e', SmartHoles) ->
        flip catchError (const default_) $
          check t e' >>= \case
            Hole{} -> default_ -- Don't let the recursive call mint a hole.
            e'' -> pure e''
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
      (False, SmartHoles) -> do
        -- Avoid automatically generated names shadowing anything
        globals <- getGlobalBaseNames
        locals <- asks $ M.keysSet . localCxt
        liftA2 (,) (mapM (createBinding (locals <> globals)) args) emptyHole
      -- otherwise, convert all @Maybe TypeCache@ metadata to @TypeCache@
      -- otherwise, annotate each binding with its type
      (True, _) ->
        let args' = zipWith (\ty bind -> (over _bindMeta (annotate (TCChkedAt ty)) bind, ty)) args patterns
         in pure (args', rhs)
    rhs' <- local (extendLocalCxts (map (first bindName) fixedPats)) $ check t fixedRHS
    pure $ CaseBranch nb (map fst fixedPats) rhs'
  where
    createBinding :: S.Set Name -> Type' () -> m (Bind' (Meta TypeCache), Type' ())
    createBinding namesInScope ty = do
      -- Avoid automatically generated names shadowing anything
      name <- freshLocalName' namesInScope
      bind <- Bind <$> meta' (TCChkedAt ty) <*> pure name
      pure (bind, ty)
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
matchArrowType (TFun _ a b) = pure (a, b)
matchArrowType _ = Nothing

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
    holepunch s t = (s, t)

-- | Compare two types for alpha equality, ignoring their IDs
eqType :: Type' a -> Type' b -> Bool
eqType t1 t2 = forgetTypeMetadata t1 `alphaEqTy` forgetTypeMetadata t2

-- | Convert @Expr (Meta Type) (Meta Kind)@ to @Expr (Meta (Maybe Type)) (Meta (Maybe Kind))@
exprTtoExpr :: ExprT -> Expr
exprTtoExpr = over _exprTypeMeta (fmap Just) . over _exprMeta (fmap Just)

-- | Convert @Type (Meta Kind)@ to @Type (Meta (Maybe Kind))@
typeTtoType :: TypeT -> Type' TypeMeta
typeTtoType = over _typeMeta (fmap Just)

checkKind' :: TypeM e m => Kind -> Type' (Meta a) -> m TypeT
checkKind' k t = modifyError' KindError (checkKind k t)

data TypeDefError
  = TDIHoleType -- a type hole
  | TDINotADT -- e.g. a function type etc
  | TDIUnknown TyConName -- not in scope
  | TDINotSaturated -- e.g. @List@ or @List a b@ rather than @List a@

data TypeDefInfo a = TypeDefInfo [Type' a] TyConName (TypeDef ()) -- instantiated parameters, and the typedef (with its name), i.e. [Int] are the parameters for @List Int@

getTypeDefInfo' :: TypeDefMap -> Type' a -> Either TypeDefError (TypeDefInfo a)
getTypeDefInfo' _ (TEmptyHole _) = Left TDIHoleType
getTypeDefInfo' tydefs (TCon _ tycon) =
      case M.lookup tycon tydefs of
        Nothing -> Left $ TDIUnknown tycon
        Just tydef
          -- this check would be redundant if we were sure that the input type
          -- were of kind KType, alternatively we should do kind checking here
          | otherwise -> Right $ TypeDefInfo [] tycon tydef
getTypeDefInfo' _ _ = Left TDINotADT

-- | Takes a particular instance of a parameterised type (e.g. @List Nat@), and
-- extracts both both the raw typedef (e.g. @List a = Nil | Cons a (List a)@)
-- and the constructors with instantiated argument types
-- (e.g. @Nil : List Nat ; Cons : Nat -> List Nat -> List Nat@)
instantiateValCons ::
  (MonadFresh NameCounter m, MonadReader Cxt m) =>
  Type' () ->
  m (Either TypeDefError (TyConName, ASTTypeDef (), [(ValConName, [Type' ()])]))
instantiateValCons t = do
  tds <- asks typeDefs
  let instCons = instantiateValCons' tds t
      -- Because @(,,) a b@ does not have a Traversable instance
      -- we reassociate so we use the one of @(,) a@
      reassoc (a, b, c) = ((a, b), c)
      reassoc' ((a, b), c) = (a, b, c)
      sequence4 =
        fmap (getCompose . getCompose . getCompose . getCompose)
          . sequence
          . Compose
          . Compose
          . Compose
          . Compose
  -- We eta-expand here to deal with simplified subsumption
  {- HLINT ignore instantiateValCons "Use id" -}
  fmap (fmap reassoc') $ sequence4 $ fmap (fmap (fmap $ fmap $ \x -> x) . reassoc) instCons

-- | As 'instantiateValCons', but pulls out the relevant bits of the monadic
-- context into an argument
instantiateValCons' ::
  TypeDefMap ->
  Type' () ->
  Either TypeDefError (TyConName, ASTTypeDef (), [(ValConName, forall m. MonadFresh NameCounter m => [m (Type' ())])])
instantiateValCons' tyDefs t =
  getTypeDefInfo' tyDefs t
    >>= \(TypeDefInfo _params tc def) -> case def of
      TypeDefAST tda -> pure (tc, tda, [])

-- | A lens for the type annotation of an 'Expr' or 'ExprT'
_typecache :: Lens' (Expr' (Meta a) b) a
_typecache = _exprMetaLens % _type

-- Helper to create fresh names
getGlobalNames :: MonadReader Cxt m => m (S.Set (ModuleName, Name))
getGlobalNames = do
  tyDefs <- asks typeDefs
  topLevel <- asks $ S.fromList . map f . M.keys . globalCxt
  let ctors =
        Map.foldMapWithKey
          ( \t _def ->
              S.fromList [f t]
          )
          tyDefs
  pure $ S.union topLevel ctors
  where
    f = qualifiedModule &&& baseName

getGlobalBaseNames :: MonadReader Cxt m => m (S.Set Name)
getGlobalBaseNames = S.map snd <$> getGlobalNames
