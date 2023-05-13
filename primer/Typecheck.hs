{-# LANGUAGE ImpredicativeTypes #-}

-- | Typechecking for Core expressions.
-- This closely follows the type system of Hazelnut, but supports arbitrary
-- types rather than just numbers.
-- In future we will want to extend it to support more features such as
-- polymorphism.
module Typecheck (
  Type,
  Expr,
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
  exprTtoExpr,
  typeTtoType,
  consistentTypes,
  extendGlobalCxt,
  extendTypeDefCxt,
) where

import Foreword

import Core (
  Expr,
  Expr' (..),
  GVarName,
  ID,
  Kind (..),
  Type' (..),
  TypeMeta,
  qualifyName,
 )
import CoreUtils (
  alphaEqTy,
  forgetTypeMetadata,
  noHoles,
 )
import DSL (S, create')
import Data.Foldable (foldMap')
import Data.Map.Strict qualified as Map
import Def (
  ASTDef (..),
  Def (..),
  DefMap,
  defType,
  _DefAST,
  _astDefType,
 )
import Fresh (MonadFresh (..))
import KindError (
  KindError (
    InconsistentKinds,
    KindDoesNotMatchArrow,
    TLetUnsupported,
    UnknownTypeConstructor
  ),
 )
import Module (
  Module (
    moduleName
  ),
  moduleDefsQualified,
  moduleTypesQualified,
  _moduleDefs,
 )
import Name (Name, NameCounter)
import NestedError (MonadNestedError (..), modifyError')
import Optics (
  A_Traversal,
  AppendIndices,
  IxFold,
  IxTraversal',
  JoinKinds,
  Optic',
  WithIx,
  equality,
  icompose,
  itoListOf,
  itraversed,
  reindexed,
  selfIndex,
  to,
  traverseOf,
  (%),
 )
import Optics.Traversal (traversed)
import TypeDef (
  TypeDefMap,
  typeDefKind,
 )
import TypeError (TypeError (..))

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

type TypeT = Type' TypeMeta

-- Synthesise a kind for the given type
synthKind :: KindM e m => Type' ID -> m (Kind, TypeT)
synthKind = \case
  TEmptyHole m -> pure (KType, TEmptyHole m)
  TCon m c -> do
    typeDef <- asks (Map.lookup c . typeDefs)
    case typeDef of
      Nothing -> throwError' $ UnknownTypeConstructor c
      Just def -> let k = typeDefKind def in pure (k, TCon m c)
  TFun m a b -> do
    a' <- checkKind KType a
    b' <- checkKind KType b
    pure (KType, TFun m a' b')

checkKind :: KindM e m => Kind -> Type' ID -> m TypeT
checkKind _ t = do
  (_, t') <- synthKind t
  pure t'

assert :: MonadNestedError TypeError e m => Bool -> Text -> m ()
assert b s = unless b $ throwError' (InternalError s)

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
synth :: TypeM e m => Expr -> m (Type, Expr)
synth = \case
  Ann i e t -> do
    -- Check that the type is well-formed by synthesising its kind
    t' <- checkKind' KType t
    let t'' = forgetTypeMetadata t'
    -- Check e against the annotation
    e' <- check t'' e
    -- Annotate the Ann with the same type as e
    pure (t'', Ann i e' t')
  EmptyHole i -> pure (TEmptyHole (), EmptyHole i)
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
    pure (TEmptyHole (), Hole i e')

-- | Similar to synth, but for checking rather than synthesis.
check :: TypeM e m => Type -> Expr -> m Expr
check t = \case
  e -> do
    sh <- asks smartHoles
    let default_ = do
          (t', e') <- synth e
          if consistentTypes t t'
            then pure e'
            else case sh of
              NoSmartHoles -> throwError' (InconsistentTypes t t')
              SmartHoles -> Hole <$> fresh <*> pure e'
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
exprTtoExpr :: Expr -> Expr
exprTtoExpr = identity

-- | Convert @Type (Meta Kind)@ to @Type (Meta (Maybe Kind))@
typeTtoType :: TypeT -> Type' TypeMeta
typeTtoType = identity

checkKind' :: TypeM e m => Kind -> Type' ID -> m TypeT
checkKind' k t = modifyError' KindError (checkKind k t)
