module Action (
  ProgAction (..),
  applyActionsToBody,
  applyActionsToTypeSig,
  toProgActionInput,
  toProgActionNoInput,
  constructArrowL,
) where

import Foreword hiding (mod)

import Actions (Action (..))
import Available (NodeType (..))
import Available qualified as Available
import Core (
  Expr (..),
  Type,
  getID,
 )
import DSL (
  ann,
  emptyHole,
  tEmptyHole,
  tfun,
 )
import Def (
  Def (..), DefMap,
 )
import Errors (Error (..))
import Fresh (MonadFresh)
import ProgAction (ProgAction (..))
import Typecheck (
  CheckEverythingRequest (CheckEverything, toCheckTys, toCheckTms),
  buildTypingContext,
  check,
  checkEverything,
  synth,
 )
import Typecheck qualified as TC
import Zipper (
  ExprZ,
  IsZipper,
  Loc (..),
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
import TypeDef (TypeDefMap)
import qualified Data.Map.Strict as M

-- | A shorthand for the constraints needed when applying actions
type ActionM m =
  ( Monad m
  , MonadFresh Int m -- can generate fresh IDs
  , MonadError Error m -- can raise errors
  , MonadReader TC.Cxt m -- has access to a typing context
  )

-- | Apply a sequence of actions to the type signature of a definition
-- We apply the actions to the type, then typecheck the body of the definition against the new type.
-- We must then typecheck the whole program to check any uses of the definition.
-- Note that this may introduce new holes when using SmartHoles, and thus we
-- return a whole set of modules as well as the one definition we wanted to
-- change.
applyActionsToTypeSig ::
  (MonadFresh Int m) =>
  TypeDefMap ->
  DefMap ->
  -- | This must be one of the definitions in the @Module@, with its correct name
  (Text, Def) ->
  [Action] ->
  m (Either Error (TypeDefMap, DefMap, TypeZ))
applyActionsToTypeSig tys tms (defName, def) actions =
  runReaderT
    go
    (buildTypingContext tys tms)
    & runExceptT
  where
    go :: ActionM m => m (TypeDefMap, DefMap, TypeZ)
    go = do
      zt <- withWrappedType (defType def) (\zt -> foldlM (flip applyActionAndSynth) (InType zt) actions)
      let t = target (top zt)
      e <- check t (defExpr def)
      let def' = def{defExpr = e, defType = t}
          tms' = M.insert defName def' tms
      -- The actions were applied to the type successfully, and the definition body has been
      -- typechecked against the new type.
      -- Now we need to typecheck the whole program again, to check any uses of the definition
      -- We make sure that the updated type is present in the global context.
      -- Here we just check the whole of the mutable prog, excluding imports.
      -- (for efficiency, we need not check the type definitions, but we do not implement this optimisation)
      checkEverything (CheckEverything{toCheckTys = tys, toCheckTms = tms'})
        >>= \(chkTy,chkTm) -> pure (chkTy, chkTm, zt)
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
  MonadFresh Int m =>
  TypeDefMap ->
  DefMap ->
  Def ->
  [Action] ->
  m (Either Error (Def, Loc))
applyActionsToBody tys tms def actions =
  go
    & flip runReaderT (buildTypingContext tys tms)
    & runExceptT
  where
    go :: ActionM m => m (Def, Loc)
    go = do
      ze <- foldlM (flip (applyActionAndCheck (defType def))) (focusLoc (defExpr def)) actions
      e' <- check (defType def) (unfocus ze)
      let def' = def{defExpr = e'}
      refocus Refocus{pre = ze, post = e'} >>= \case
        Nothing -> throwError $ InternalFailure "lost ID after typechecking"
        Just z -> pure (def', z)

applyActionAndCheck :: ActionM m => Type -> Action -> Loc -> m Loc
applyActionAndCheck ty action z = do
  z' <- applyAction' action z
  typedAST <- check ty $ unfocus z'
  -- Refocus on where we were previously
  refocus Refocus{pre = z', post = typedAST} >>= \case
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
  refocus Refocus{pre = z, post = typedAST}

applyAction' :: ActionM m => Action -> Loc -> m Loc
applyAction' a = case a of
  Delete -> \case
    InExpr ze -> InExpr . flip replace ze <$> emptyHole
    InType zt -> InType . flip replace zt <$> tEmptyHole
  ConstructArrowL -> typeAction constructArrowL "cannot construct arrow - not in type"
  where
    typeAction f s = \case
      InType zt -> InType <$> f zt
      _ -> throwError $ CustomFailure a s

setCursor :: ActionM m => Int -> ExprZ -> m Loc
setCursor i e = case focusOn i (unfocusExpr e) of
  Just e' -> pure e'
  Nothing -> throwError $ IDNotFound i

constructArrowL :: (IsZipper z Type, MonadFresh Int m) => z -> m z
constructArrowL zt = flip replace zt <$> tfun (pure (target zt)) tEmptyHole

-- | Convert a high-level 'Available.NoInputAction' to a concrete sequence of 'ProgAction's.
toProgActionNoInput ::
  Text ->
  Maybe (NodeType, Int) ->
  Available.NoInputAction ->
  Either Error ProgAction
toProgActionNoInput defName mNodeSel = \case
  Available.MakeFun ->
    -- We arbitrarily choose that the "construct a function type" action places the focused expression
    -- on the domain (left) side of the arrow.
    toProgAction ConstructArrowL
  Available.DeleteType ->
    toProgAction Delete
  Available.DeleteDef ->
    pure $ DeleteDef defName
  where
    toProgAction actions = toProg' actions defName <$> maybeToEither NoNodeSelection mNodeSel

-- | Convert a high-level 'Available.InputAction', and associated 'Available.Option',
-- to a concrete sequence of 'ProgAction's.
toProgActionInput ::
  Text ->
  Available.Option ->
  Available.InputAction ->
  Either Error [ProgAction]
toProgActionInput defName opt = \case
  Available.RenameDef -> pure [RenameDef defName $ Available.option opt]

toProg' :: Action -> Text -> (NodeType, Int) -> ProgAction
toProg' actions defName (nt, id) = case nt of
    SigNode -> SigAction defName id actions
    BodyNode -> BodyAction defName id actions
