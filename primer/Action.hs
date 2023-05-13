module Action (
  Action (..),
  ActionError (..),
  Movement (..),
  ProgAction (..),
  applyActionsToBody,
  applyActionsToTypeSig,
  toProgActionInput,
  toProgActionNoInput,
) where

import Foreword hiding (mod)

import Actions (Action (..), Movement (..))
import Available (NodeType (..))
import Available qualified as Available
import Core (
  Expr,
  Expr' (..),
  GVarName,
  ID,
  Type,
  getID,
 )
import CoreUtils (forgetTypeMetadata)
import DSL (
  ann,
  emptyHole,
  tEmptyHole,
  tfun,
 )
import Def (
  ASTDef (..),
  Def (..),
 )
import Errors (ActionError (..))
import Fresh (MonadFresh)
import Module (Module, insertDef)
import Name (Name)
import ProgAction (ProgAction (..))
import Typecheck (
  CheckEverythingRequest (CheckEverything, toCheck),
  buildTypingContextFromModules,
  check,
  checkEverything,
  exprTtoExpr,
  synth,
 )
import Typecheck qualified as TC
import Zipper (
  ExprZ,
  Loc,
  Loc' (..),
  TypeZ,
  focus,
  focusLoc,
  focusOn,
  focusType,
  replace,
  target,
  top,
  unfocus,
  unfocusExpr,
  unfocusLoc,
 )

-- | A shorthand for the constraints needed when applying actions
type ActionM m =
  ( Monad m
  , MonadFresh ID m -- can generate fresh IDs
  , MonadError ActionError m -- can raise errors
  , MonadReader TC.Cxt m -- has access to a typing context
  )

-- | Apply a sequence of actions to the type signature of a definition
-- We apply the actions to the type, then typecheck the body of the definition against the new type.
-- We must then typecheck the whole program to check any uses of the definition.
-- Note that this may introduce new holes when using SmartHoles, and thus we
-- return a whole set of modules as well as the one definition we wanted to
-- change.
applyActionsToTypeSig ::
  (MonadFresh ID m) =>
  Module ->
  -- | This must be one of the definitions in the @Module@, with its correct name
  (Name, ASTDef) ->
  [Action] ->
  m (Either ActionError (Module, TypeZ))
applyActionsToTypeSig mod (defName, def) actions =
  runReaderT
    go
    (buildTypingContextFromModules [mod])
    & runExceptT
  where
    go :: ActionM m => m (Module, TypeZ)
    go = do
      zt <- withWrappedType (astDefType def) (\zt -> foldlM (flip applyActionAndSynth) (InType zt) actions)
      let t = target (top zt)
      e <- check (forgetTypeMetadata t) (astDefExpr def)
      let def' = def{astDefExpr = exprTtoExpr e, astDefType = t}
          mod' = insertDef mod defName (DefAST def')
      -- The actions were applied to the type successfully, and the definition body has been
      -- typechecked against the new type.
      -- Now we need to typecheck the whole program again, to check any uses of the definition
      -- We make sure that the updated type is present in the global context.
      -- Here we just check the whole of the mutable prog, excluding imports.
      -- (for efficiency, we need not check the type definitions, but we do not implement this optimisation)
      checkEverything (CheckEverything{toCheck = mod'})
        >>= \checkedMod -> pure (checkedMod, zt)
    -- Actions expect that all ASTs have a top-level expression of some sort.
    -- Signatures don't have this: they're just a type.
    -- We fake it by wrapping the type in a top-level annotation node, then unwrapping afterwards.
    withWrappedType :: ActionM m => Type -> (TypeZ -> m Loc) -> m TypeZ
    withWrappedType ty f = do
      wrappedType <- ann emptyHole (pure ty)
      let unwrapError = throwError $ InternalFailure "applyActionsToTypeSig: failed to unwrap type"
          wrapError = throwError $ InternalFailure "applyActionsToTypeSig: failed to wrap type"
          focusedType = focusType $ focus wrappedType
      case focusedType of
        -- This should be impossible
        Nothing -> wrapError
        Just wrappedTy ->
          f wrappedTy >>= \case
            InType zt -> pure zt
            -- This probably shouldn't happen, but it may be the case that an action accidentally
            -- exits the type and ends up in the outer expression that we have created as a wrapper.
            -- In this case we just refocus on the top of the type.
            z -> maybe unwrapError pure (focusType (unfocusLoc z))

data Refocus = Refocus
  { pre :: Loc
  , post :: Expr
  }

-- If smartholes is on, we may refocus on the interior of an elided hole,
-- or the expression under an elided annotation
refocus :: MonadReader TC.Cxt m => Refocus -> m (Maybe Loc)
refocus Refocus{pre, post} = do
  let candidateIDs = case pre of
          InExpr e -> candidateIDsExpr $ target e
          InType t -> candidateIDsType $ target t
  pure . getFirst . mconcat $ fmap (\i -> First $ focusOn i post) candidateIDs
  where
    candidateIDsExpr e =
      getID e : case e of
        Hole _ e' -> candidateIDsExpr e'
        Ann _ e' _ -> candidateIDsExpr e'
        _ -> []
    candidateIDsType = (: []) . getID

-- | Apply a sequence of actions to the body of a definition, producing a new Expr or an error if
-- any of the actions failed to apply.
-- After applying the actions, we check the new Expr against the type sig of the definition.
applyActionsToBody ::
  MonadFresh ID m =>
  Module ->
  ASTDef ->
  [Action] ->
  m (Either ActionError (ASTDef, Loc))
applyActionsToBody mod def actions =
  go
    & flip runReaderT (buildTypingContextFromModules [mod])
    & runExceptT
  where
    go :: ActionM m => m (ASTDef, Loc)
    go = do
      ze <- foldlM (flip (applyActionAndCheck (astDefType def))) (focusLoc (astDefExpr def)) actions
      e' <- exprTtoExpr <$> check (forgetTypeMetadata (astDefType def)) (unfocus ze)
      let def' = def{astDefExpr = e'}
      refocus Refocus{pre = ze, post = e'} >>= \case
        Nothing -> throwError $ InternalFailure "lost ID after typechecking"
        Just z -> pure (def', z)

applyActionAndCheck :: ActionM m => Type -> Action -> Loc -> m Loc
applyActionAndCheck ty action z = do
  z' <- applyAction' action z
  typedAST <- check (forgetTypeMetadata ty) $ unfocus z'
  -- Refocus on where we were previously
  refocus Refocus{pre = z', post = exprTtoExpr typedAST} >>= \case
    Just z'' -> pure z''
    Nothing -> throwError $ CustomFailure action "internal error: lost ID after typechecking"

applyActionAndSynth :: ActionM m => Action -> Loc -> m Loc
applyActionAndSynth action z = do
  z' <- applyAction' action z
  synthZ z' >>= \case
    Just z'' -> pure z''
    Nothing -> throwError $ CustomFailure action "internal error: lost ID after typechecking"

-- There's some fiddly manipulations here because the output of the typechecker
-- is @Expr' (Meta Type) (Meta Kind)@ but we need
-- @Expr' (Meta (Maybe Type)) (Meta (Maybe Kind))@, i.e. we need to wrap each
-- type and kind annotation in @Just@.
--
-- 'Nothing' means the current focussed ID disappeared after typechecking
synthZ :: ActionM m => Loc -> m (Maybe Loc)
synthZ z = do
  (_, typedAST) <- synth $ unfocus z
  -- Refocus on where we were previously
  refocus Refocus{pre = z, post = exprTtoExpr typedAST}

applyAction' :: ActionM m => Action -> Loc -> m Loc
applyAction' a = case a of
  SetCursor i -> setCursor i . unfocusLoc
  Delete -> \case
    InExpr ze -> InExpr . flip replace ze <$> emptyHole
    InType zt -> InType . flip replace zt <$> tEmptyHole
  ConstructArrowL -> typeAction constructArrowL "cannot construct arrow - not in type"
  where
    typeAction f s = \case
      InType zt -> InType <$> f zt
      _ -> throwError $ CustomFailure a s

setCursor :: ActionM m => ID -> ExprZ -> m Loc
setCursor i e = case focusOn i (unfocusExpr e) of
  Just e' -> pure e'
  Nothing -> throwError $ IDNotFound i

constructArrowL :: ActionM m => TypeZ -> m TypeZ
constructArrowL zt = flip replace zt <$> tfun (pure (target zt)) tEmptyHole

-- | Convert a high-level 'Available.NoInputAction' to a concrete sequence of 'ProgAction's.
toProgActionNoInput ::
  GVarName ->
  Maybe (NodeType, ID) ->
  Available.NoInputAction ->
  Either ActionError [ProgAction]
toProgActionNoInput defName mNodeSel = \case
  Available.MakeFun ->
    -- We arbitrarily choose that the "construct a function type" action places the focused expression
    -- on the domain (left) side of the arrow.
    toProgAction [ConstructArrowL]
  Available.DeleteType ->
    toProgAction [Delete]
  Available.DeleteDef ->
    pure [DeleteDef defName]
  where
    toProgAction actions = toProg' actions defName <$> maybeToEither NoNodeSelection mNodeSel

-- | Convert a high-level 'Available.InputAction', and associated 'Available.Option',
-- to a concrete sequence of 'ProgAction's.
toProgActionInput ::
  GVarName ->
  Available.Option ->
  Available.InputAction ->
  Either ActionError [ProgAction]
toProgActionInput defName opt = \case
  Available.RenameDef -> pure [RenameDef defName $ Available.option opt]

toProg' :: [Action] -> GVarName -> (NodeType, ID) -> [ProgAction]
toProg' actions defName (nt, id) =
  [ MoveToDef defName
  , (SetCursor id : actions) & case nt of
      SigNode -> SigAction
      BodyNode -> BodyAction
  ]
