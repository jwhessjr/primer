module Primer.Action (
  Action (..),
  ActionError (..),
  Movement (..),
  ProgAction (..),
  applyActionsToBody,
  applyActionsToTypeSig,
  moveExpr,
  toProgActionInput,
  toProgActionNoInput,
) where

import Foreword hiding (mod)

import Control.Monad.Fresh (MonadFresh)
import Data.List (findIndex)
import Primer.Action.Actions (Action (..), Movement (..))
import Primer.Action.Available qualified as Available
import Primer.Action.Errors (ActionError (..))
import Primer.Action.ProgAction (ProgAction (..))
import Primer.App.Base (NodeType (..))
import Primer.Core (
  Expr,
  Expr' (..),
  GVarName,
  HasID,
  ID,
  Type,
  Type' (..),
  getID,
 )
import Primer.Core qualified as C
import Primer.Core.DSL (
  ann,
  emptyHole,
  tEmptyHole,
  tfun,
 )
import Primer.Core.Utils (forgetTypeMetadata)
import Primer.Def (
  ASTDef (..),
  Def (..),
 )
import Primer.Module (Module, insertDef)
import Primer.Name (Name, NameCounter)
import Primer.Typecheck (
  CheckEverythingRequest (CheckEverything, toCheck, trusted),
  SmartHoles,
  buildTypingContextFromModules,
  check,
  checkEverything,
  exprTtoExpr,
  synth,
 )
import Primer.Typecheck qualified as TC
import Primer.Zipper (
  BindLoc' (..),
  ExprZ,
  IsZipper,
  Loc,
  Loc' (..),
  TypeZ,
  down,
  focus,
  focusLoc,
  focusOn,
  focusType,
  replace,
  right,
  target,
  top,
  unfocus,
  unfocusExpr,
  unfocusLoc,
  up,
 )

-- | A shorthand for the constraints needed when applying actions
type ActionM m =
  ( Monad m
  , MonadFresh ID m -- can generate fresh IDs
  , MonadFresh NameCounter m -- can generate fresh names
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
  (MonadFresh ID m, MonadFresh NameCounter m) =>
  SmartHoles ->
  [Module] ->
  -- | The @Module@ we are focused on, and all the other editable modules
  (Module, [Module]) ->
  -- | This must be one of the definitions in the @Module@, with its correct name
  (Name, ASTDef) ->
  [Action] ->
  m (Either ActionError ([Module], TypeZ))
applyActionsToTypeSig smartHoles _imports (mod, mods) (defName, def) actions =
  runReaderT
    go
    (buildTypingContextFromModules (mod : mods) smartHoles)
    & runExceptT
  where
    go :: ActionM m => m ([Module], TypeZ)
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
      checkEverything smartHoles (CheckEverything{trusted = [], toCheck = mod' : mods})
        >>= \checkedMods -> pure (checkedMods, zt)
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
  sh <- asks TC.smartHoles
  let candidateIDs = case sh of
        TC.NoSmartHoles -> [getID pre]
        TC.SmartHoles -> case pre of
          InExpr e -> candidateIDsExpr $ target e
          InType t -> candidateIDsType $ target t
          InBind (BindCase ze) -> [getID ze]
  pure . getFirst . mconcat $ fmap (\i -> First $ focusOn i post) candidateIDs
  where
    candidateIDsExpr e =
      getID e : case e of
        Ann _ e' _ -> candidateIDsExpr e'
        _ -> []
    candidateIDsType = (:[]) . getID

-- | Apply a sequence of actions to the body of a definition, producing a new Expr or an error if
-- any of the actions failed to apply.
-- After applying the actions, we check the new Expr against the type sig of the definition.
applyActionsToBody ::
  (MonadFresh ID m, MonadFresh NameCounter m) =>
  SmartHoles ->
  [Module] ->
  ASTDef ->
  [Action] ->
  m (Either ActionError (ASTDef, Loc))
applyActionsToBody sh modules def actions =
  go
    & flip runReaderT (buildTypingContextFromModules modules sh)
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
  Move m -> \case
    InExpr z -> InExpr <$> moveExpr m z
    InType z -> InType <$> moveType m z
    z@(InBind _) -> case m of
      -- If we're moving up from a binding, then shift focus to the nearest parent expression.
      -- This is exactly what 'unfocusLoc' does if the 'Loc' is a binding.
      Parent -> pure . InExpr $ unfocusLoc z
      _ -> throwError $ CustomFailure (Move m) "Can only move up from a binding"
  Delete -> \case
    InExpr ze -> InExpr . flip replace ze <$> emptyHole
    InType zt -> InType . flip replace zt <$> tEmptyHole
    InBind _ -> throwError $ CustomFailure Delete "Cannot delete a binding"
  ConstructArrowL -> typeAction constructArrowL "cannot construct arrow - not in type"
  where
    typeAction f s = \case
      InType zt -> InType <$> f zt
      _ -> throwError $ CustomFailure a s

setCursor :: ActionM m => ID -> ExprZ -> m Loc
setCursor i e = case focusOn i (unfocusExpr e) of
  Just e' -> pure e'
  Nothing -> throwError $ IDNotFound i

-- | Apply a movement to a zipper
moveExpr :: ActionM m => Movement -> ExprZ -> m ExprZ
moveExpr m@(Branch c) z | Case _ _ brs <- target z =
  case findIndex (\(C.CaseBranch n _ _) -> c == n) brs of
    Nothing -> throwError $ CustomFailure (Move m) "Move-to-branch failed: no such branch"
    -- 'down' moves into the scrutinee, 'right' then steps through branch
    -- rhss
    Just i -> case foldr (\_ z' -> right =<< z') (down z) [0 .. i] of
      Just z' -> pure z'
      Nothing -> throwError $ CustomFailure (Move m) "internal error: movement failed, even though branch exists"
moveExpr m@(Branch _) _ = throwError $ CustomFailure (Move m) "Move-to-branch failed: this is not a case expression"
moveExpr Child2 z
  | Case{} <- target z =
      throwError $ CustomFailure (Move Child2) "cannot move to 'Child2' of a case: use Branch instead"
moveExpr m z = move m z

-- | Apply a movement to a zipper
moveType :: ActionM m => Movement -> TypeZ -> m TypeZ
moveType m@(Branch _) _ = throwError $ CustomFailure (Move m) "Move-to-branch unsupported in types (there are no cases in types!)"
moveType m z = move m z

-- | Apply a movement to a generic zipper - does not support movement to a case
-- branch
move :: forall m za a. (ActionM m, IsZipper za a, HasID za) => Movement -> za -> m za
move m z = do
  mz' <- move' m z
  case mz' of
    Just z' -> pure z'
    Nothing -> throwError $ MovementFailed (getID z, m)
  where
    move' :: Movement -> za -> m (Maybe za)
    move' Parent = pure . up
    move' Child1 = pure . down
    move' Child2 = pure . (down >=> right)
    move' (Branch _) = const $ throwError $ InternalFailure "move does not support Branch moves"

constructArrowL :: ActionM m => TypeZ -> m TypeZ
constructArrowL zt = flip replace zt <$> tfun (pure (target zt)) tEmptyHole

-- | Convert a high-level 'Available.NoInputAction' to a concrete sequence of 'ProgAction's.
toProgActionNoInput ::
  GVarName ->
  Maybe (NodeType, ID) ->
  Available.NoInputAction ->
  Either ActionError [ProgAction]
toProgActionNoInput defName mNodeSel = \case
  Available.Raise -> do
    id <- mid
    pure [MoveToDef defName, CopyPasteBody (defName, id) [SetCursor id, Move Parent, Delete]]
  Available.MakeFun ->
    -- We arbitrarily choose that the "construct a function type" action places the focused expression
    -- on the domain (left) side of the arrow.
    toProgAction [ConstructArrowL, Move Child1]
  Available.DeleteType ->
    toProgAction [Delete]
  Available.DeleteDef ->
    pure [DeleteDef defName]
  where
    toProgAction actions = toProg' actions defName <$> maybeToEither NoNodeSelection mNodeSel
    mid = maybeToEither NoNodeSelection $ snd <$> mNodeSel

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
