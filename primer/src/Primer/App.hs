-- This module defines the high level application functions.

module Primer.App (
  module Primer.Action.Available,
  Log (..),
  App,
  mkApp,
  appProg,
  appIdCounter,
  appNameCounter,
  appInit,
  EditAppM,
  QueryAppM,
  runEditAppM,
  Prog (..),
  progAllModules,
  progAllDefs,
  tcWholeProg,
  tcWholeProgWithImports,
  ProgAction (..),
  ProgError (..),
  handleMutationRequest,
  handleEditRequest,
  MutationRequest (..),
  Selection (..),
  NodeSelection (..),
  lookupASTDef,
  liftError,
) where

import Foreword hiding (mod)

import Control.Monad.Fresh (MonadFresh (..))
import Control.Monad.NestedError (MonadNestedError)
import Data.Data (Data)
import Data.Foldable (foldMap')
import Data.Generics.Uniplate.Zipper (
  fromZipper,
 )
import Data.Map.Strict qualified as Map
import Optics (
  ReversibleOptic (re),
  Setter',
  lens,
  over,
  set,
  sets,
  traverseOf,
  traversed,
  view,
  (%),
  (.~),
  (?~),
  (^.),
  _Left,
  _Right, Getter, to,
 )
import Primer.Action (
  Action,
  ActionError (..),
  ProgAction (..),
  applyActionsToBody,
  applyActionsToTypeSig,
 )
import Primer.Action.ProgError (ProgError (..))
import Primer.Action.Available (
  Editable (..),
  Level (..),
  NodeType (..),
 )
import Primer.Core (
  Expr' (EmptyHole),
  ExprMeta,
  GVarName,
  GlobalName (baseName, qualifiedModule),
  HasID (_id),
  ID (..),
  ModuleName,
  TmVarRef (GlobalVarRef),
  Type' (..),
  TypeMeta,
  getID,
  qualifyName,
  _exprMetaLens,
  _typeMetaLens,
 )
import Primer.Core.Transform (renameVar)
import Primer.Core.Utils (regenerateExprIDs, regenerateTypeIDs)
import Primer.Def (
  ASTDef (..),
  _astDefExpr,
  Def (..),
  _DefAST,
  DefMap,
  defAST,
 )
import Primer.Def.Utils (globalInUse)
import Primer.Module (
  Module (moduleDefs, moduleName),
  _moduleDefs,
  deleteDef,
  insertDef,
  moduleDefsQualified,
  qualifyDefName,
 )
import Primer.Name (Name, NameCounter, unsafeMkName)
import Primer.Typecheck (
  CheckEverythingRequest (CheckEverything, toCheck, trusted),
  Cxt,
  SmartHoles,
  TypeError,
  checkEverything,
 )
import Primer.Zipper (
  ExprZ,
  Loc' (InBind, InExpr, InType),
  TypeZ,
  TypeZip,
  focusOn,
  focusOnTy,
  focusOnlyType,
  locToEither,
  replace,
  target,
  unfocusExpr,
  unfocusType,
  _target,
 )

-- | The full program state.
data Prog = Prog
  { progModules :: [Module]
  -- ^ The editable "home" modules
  , progSelection :: Maybe Selection
  , progSmartHoles :: SmartHoles
  , progLog :: Log
  -- ^ The sequence of all successful edits performed on the program
  -- since its creation, in order of first edit to last. A successful
  -- undo operation pops the last edit from this log, and pushes it
  -- onto the 'redoLog'.
  }
  deriving stock (Eq, Show, Read)

_progSelection :: Setter' Prog (Maybe Selection)
_progSelection = sets $ \f p -> p {progSelection = f $ progSelection p}

-- | Push a compound action onto the given 'Log', returning the new
-- 'Log'.
push :: [ProgAction] -> Log -> Log
push as l = Log $ as : unlog l

progAllModules :: Prog -> [Module]
progAllModules p = progModules p

progAllDefs :: Prog -> Map GVarName (Editable, Def)
progAllDefs p = foldMap' (fmap (Editable,) . moduleDefsQualified) (progModules p)

-- Note [Modules]
-- The invariant is that the @progImports@ modules are never edited, but
-- one can insert new ones (and perhaps delete unneeded ones).
--
-- We assume that all Names and IDs are unique within one module, and that
-- module names are unique.
--
-- All modules in a @Prog@ shall be well-typed, in the appropriate scope:
-- all the imports are in one mutual dependency group
-- the @progModule@ has all the imports in scope

-- | Get all definitions from all modules (including imports)
allDefs :: Prog -> DefMap
allDefs = fmap snd . progAllDefs

-- | The action log
--  This is the canonical store of the program - we can recreate any current or
--  past program state by replaying this log.
--  Each item is a sequence of Core actions which should be applied atomically.
--  Items are stored in reverse order so it's quick to add new ones.
newtype Log = Log {unlog :: [[ProgAction]]}
  deriving stock (Eq, Show, Read)
  deriving newtype (Semigroup, Monoid)

-- | Describes what interface element the user has selected.
-- A definition in the left hand nav bar, and possibly a node in that definition.
data Selection = Selection
  { selectedDef :: GVarName
  -- ^ the ID of some ASTDef
  , selectedNode :: Maybe NodeSelection
  }
  deriving stock (Eq, Show, Read, Data)

_selectedDef :: Getter Selection GVarName
_selectedDef = to selectedDef

_selectedNode :: Getter Selection (Maybe NodeSelection)
_selectedNode = to selectedNode

-- | A selected node, in the body or type signature of some definition.
-- We have the following invariant: @nodeType = SigNode ==> isRight meta@
data NodeSelection = NodeSelection
  { nodeType :: NodeType
  , meta :: Either ExprMeta TypeMeta
  }
  deriving stock (Eq, Show, Read, Data)

_meta :: Setter' NodeSelection (Either ExprMeta TypeMeta)
_meta = sets $ \f ns -> ns {meta = f $ meta ns}

instance HasID NodeSelection where
  _id =
    lens
      (either getID getID . meta)
      (flip $ \id -> over _meta $ bimap (set _id id) (set _id id))

-- | The type of requests which can mutate the application state.
data MutationRequest
  = Undo
  | Edit [ProgAction]
  deriving stock (Eq, Show, Read)

-- * Request handlers

-- This only looks in the editable modules, not in any imports
focusNode :: MonadError ProgError m => Prog -> GVarName -> ID -> m (Either (Either ExprZ TypeZ) TypeZip)
focusNode prog = focusNodeDefs $ foldMap' moduleDefsQualified $ progModules prog

-- This looks in the editable modules and also in any imports
focusNodeImports :: MonadError ProgError m => Prog -> GVarName -> ID -> m (Either (Either ExprZ TypeZ) TypeZip)
focusNodeImports prog = focusNodeDefs $ allDefs prog

focusNodeDefs :: MonadError ProgError m => DefMap -> GVarName -> ID -> m (Either (Either ExprZ TypeZ) TypeZip)
focusNodeDefs defs defname nodeid =
  case lookupASTDef defname defs of
    Nothing -> throwError $ DefNotFound defname
    Just def ->
      let mzE = locToEither <$> focusOn nodeid (astDefExpr def)
          mzT = focusOnTy nodeid $ astDefType def
       in case fmap Left mzE <|> fmap Right mzT of
            Nothing -> throwError $ ActionError (IDNotFound nodeid)
            Just x -> pure x

-- | Handle a request to mutate the app state
handleMutationRequest :: MonadEditApp l ProgError m => MutationRequest -> m Prog
handleMutationRequest = \case
  Edit as -> handleEditRequest as
  Undo -> handleUndoRequest

-- | Handle an edit request
--
-- Note that a successful edit resets the redo log.
handleEditRequest :: forall m l. MonadEditApp l ProgError m => [ProgAction] -> m Prog
handleEditRequest actions = do
  (prog, _) <- gets appProg >>= \p -> foldlM go (p, Nothing) actions
  let l = progLog prog
  let prog' = prog{progLog = push actions l}
  modify (\s -> s & _currentState % _prog .~ prog')
  pure prog'
  where
    go :: (Prog, Maybe GVarName) -> ProgAction -> m (Prog, Maybe GVarName)
    go (prog, mdef) a =
      applyProgAction prog mdef a <&> \prog' ->
        (prog', selectedDef <$> progSelection prog')

-- | Handle a 'ProgAction'
-- The 'GVarName' argument is the currently-selected definition, which is
-- provided for convenience: it is the same as the one in the progSelection.
applyProgAction :: MonadEdit m ProgError => Prog -> Maybe GVarName -> ProgAction -> m Prog
applyProgAction prog mdefName = \case
  MoveToDef d -> do
    m <- lookupEditableModule (qualifiedModule d) prog
    case Map.lookup d $ moduleDefsQualified m of
      Nothing -> throwError $ DefNotFound d
      Just _ -> pure $ prog & _progSelection ?~ Selection d Nothing
  DeleteDef d -> editModuleCross (qualifiedModule d) prog $ \(m, ms) ->
    case deleteDef m d of
      Nothing -> throwError $ DefNotFound d
      Just mod' -> do
        when (globalInUse d $ foldMap' moduleDefs $ mod' : ms) $
          throwError $
            DefInUse d
        pure (mod' : ms, Nothing)
  RenameDef d nameStr -> editModuleOfCross (Just d) prog $ \(m, ms) defName def -> do
    let defs = moduleDefs m
        newNameBase = unsafeMkName nameStr
        newName = qualifyName (moduleName m) newNameBase
    if Map.member newNameBase defs
      then throwError $ DefAlreadyExists newName
      else do
        let m' = m{moduleDefs = Map.insert newNameBase (DefAST def) $ Map.delete defName defs}
        renamedModules <-
          maybe (throwError $ ActionError NameCapture) pure $
            traverseOf
              (traversed % _moduleDefs % traversed % _DefAST % _astDefExpr)
              (renameVar (GlobalVarRef d) (GlobalVarRef newName))
              (m' : ms)
        pure (renamedModules, Just $ Selection newName Nothing)
  BodyAction actions -> editModuleOf mdefName prog $ \m defName def -> do
    let smartHoles = progSmartHoles prog
    res <- applyActionsToBody smartHoles (progAllModules prog) def actions
    case res of
      Left err -> throwError $ ActionError err
      Right (def', z) -> do
        let meta = bimap (view _exprMetaLens . target) (view _typeMetaLens . target) $ locToEither z
        pure
          ( insertDef m defName (DefAST def')
          , Just $
              Selection (qualifyDefName m defName) $
                Just
                  NodeSelection
                    { nodeType = BodyNode
                    , meta
                    }
          )
  SigAction actions -> editModuleOfCross mdefName prog $ \ms@(curMod, _) defName def -> do
    let smartHoles = progSmartHoles prog
    res <- applyActionsToTypeSig smartHoles [] ms (defName, def) actions
    case res of
      Left err -> throwError $ ActionError err
      Right (mod', zt) -> do
        let node = target zt
            meta = view _typeMetaLens node
         in pure
              ( mod'
              , Just $
                  Selection (qualifyDefName curMod defName) $
                    Just
                      NodeSelection
                        { nodeType = SigNode
                        , meta = Right meta
                        }
              )
  CopyPasteSig fromIds setup -> case mdefName of
    Nothing -> throwError NoDefSelected
    Just i -> copyPasteSig prog fromIds i setup
  CopyPasteBody fromIds setup -> case mdefName of
    Nothing -> throwError NoDefSelected
    Just i -> copyPasteBody prog fromIds i setup

lookupEditableModule :: MonadError ProgError m => ModuleName -> Prog -> m Module
lookupEditableModule n p =
  lookupModule' n p >>= \case
    MLEditable m -> pure m

-- | Describes return type of successfully looking a module up in the program.
-- We get the module and also whether it is imported or not.
data ModuleLookup = MLEditable Module

lookupModule' :: MonadError ProgError m => ModuleName -> Prog -> m ModuleLookup
lookupModule' n p = case find ((n ==) . moduleName) (progModules p) of
  Just m -> pure $ MLEditable m
  Nothing -> throwError $ ModuleNotFound n

editModule ::
  MonadError ProgError m =>
  ModuleName ->
  Prog ->
  (Module -> m (Module, Maybe Selection)) ->
  m Prog
editModule n p f = do
  m <- lookupEditableModule n p
  (m', s) <- f m
  pure $
    p
      { progModules = m' : filter ((/= n) . moduleName) (progModules p)
      , progSelection = s
      }

-- A variant of 'editModule' for actions which can affect multiple modules
editModuleCross ::
  MonadError ProgError m =>
  ModuleName ->
  Prog ->
  ((Module, [Module]) -> m ([Module], Maybe Selection)) ->
  m Prog
editModuleCross n p f = do
  m <- lookupEditableModule n p
  let otherModules = filter ((/= n) . moduleName) (progModules p)
  (m', s) <- f (m, otherModules)
  pure $
    p
      { progModules = m'
      , progSelection = s
      }

editModuleOf ::
  MonadError ProgError m =>
  Maybe GVarName ->
  Prog ->
  (Module -> Name -> ASTDef -> m (Module, Maybe Selection)) ->
  m Prog
editModuleOf mdefName prog f = case mdefName of
  Nothing -> throwError NoDefSelected
  Just defname -> editModule (qualifiedModule defname) prog $ \m ->
    case Map.lookup (baseName defname) (moduleDefs m) of
      Just (DefAST def) -> f m (baseName defname) def
      _ -> throwError $ DefNotFound defname

-- A variant of 'editModuleOf' for actions which can affect multiple modules
editModuleOfCross ::
  MonadError ProgError m =>
  Maybe GVarName ->
  Prog ->
  ((Module, [Module]) -> Name -> ASTDef -> m ([Module], Maybe Selection)) ->
  m Prog
editModuleOfCross mdefName prog f = case mdefName of
  Nothing -> throwError NoDefSelected
  Just defname -> editModuleCross (qualifiedModule defname) prog $ \ms@(m, _) ->
    case Map.lookup (baseName defname) (moduleDefs m) of
      Just (DefAST def) -> f ms (baseName defname) def
      _ -> throwError $ DefNotFound defname

-- | Undo the last block of actions.
--
-- If there are no actions in the log we return the program unchanged.
--
-- Otherwise, we undo by replaying the whole log from the start.
-- Because actions often refer to the IDs of nodes created by previous
-- actions we must reset the ID and name counter to their original
-- state before we replay. We do this by resetting the entire app
-- state.
--
-- If the replay is successful, then we return the new program and
-- push the block of actions that were undone onto the redo log.
--
-- If the replay is unsuccessful, then we throw a 'ProgError' and
-- leave it to the caller to decide how to handle it.
handleUndoRequest :: MonadEditApp l ProgError m => m Prog
handleUndoRequest = do
  prog <- gets appProg
  start <- gets appInit
  case unlog (progLog prog) of
    [] -> pure prog
    (_ : as) -> do
      runEditAppM (replay (reverse as)) start >>= \case
        (Right _, app') -> do
          put app'
          gets appProg
        (Left err, _) -> throwError err

-- Replay a series of actions, updating the app state with the new program
replay :: MonadEditApp l ProgError m => [[ProgAction]] -> m ()
replay = mapM_ handleEditRequest

-- | A shorthand for the constraints we need when performing mutation
-- operations on the application.
--
-- Note we do not want @MonadFresh Name m@, as @fresh :: m Name@ has
-- no way of avoiding user-specified names. Instead, use 'freshName'.
type MonadEditApp l e m = (MonadEdit m e, MonadState App m)

-- | A shorthand for constraints needed when doing low-level mutation
-- operations which do not themselves update the 'App' contained in a
-- 'State' monad. (Typically interaction with the @State@ monad would
-- be handled by a caller.
type MonadEdit m e = (MonadFresh ID m, MonadFresh NameCounter m, MonadError e m)

-- | A shorthand for the constraints we need when performing read-only
-- operations on the application.

-- | The 'EditApp' monad.
--
-- Actions run in this monad can modify the 'App'. 'ExceptT' wraps
-- state so that an action that throws an error does not modify the
-- state. This is important to ensure that we can reliably replay the
-- log without having ID mismatches.
newtype EditAppM m e a = EditAppM (StateT App (ExceptT e m) a)
  deriving newtype (Functor, Applicative, Monad, MonadState App, MonadError e)

-- | Run an 'EditAppM' action, returning a result and an updated
-- 'App'.
runEditAppM :: Functor m => EditAppM m e a -> App -> m (Either e a, App)
runEditAppM (EditAppM m) appState =
  runExceptT (runStateT m appState) <&> \case
    Left err -> (Left err, appState)
    Right (res, appState') -> (Right res, appState')

-- | The 'QueryApp' monad.
--
-- Actions run in this monad cannot modify the 'App'. We use 'ExceptT'
-- here for compatibility with 'EditApp'.
newtype QueryAppM a = QueryAppM (ReaderT App (Except ProgError) a)
  deriving newtype (Functor, Applicative, Monad, MonadReader App, MonadError ProgError)

-- | The student's application's state.
--
-- Building an 'App' can be tricky, so we don't export the
-- constructor. See 'mkApp' and 'mkAppSafe'.
data App = App
  { currentState :: AppState
  , initialState :: AppState
  }
  deriving stock (Eq, Show, Read)

_currentState :: Setter' App AppState
_currentState = sets $ \f a -> a {currentState = f $ currentState a}

-- Internal app state. Note that this type is not exported, as we want
-- to guarantee that the counters are kept in sync with the 'Prog',
-- and this should only be done via the 'MonadFresh' instances in this
-- module.
data AppState = AppState
  { idCounter :: ID
  , nameCounter :: NameCounter
  , prog :: Prog
  }
  deriving stock (Eq, Show, Read)

_idCounter :: Setter' AppState ID
_idCounter = sets $ \f as -> as {idCounter = f $ idCounter as}

_nameCounter :: Setter' AppState NameCounter
_nameCounter = sets $ \f as -> as {nameCounter = f $ nameCounter as}

_prog :: Setter' AppState Prog
_prog = sets $ \f as -> as {prog = f $ prog as}

-- | Construct an 'App' from an 'ID' and a 'Prog'.
--
-- Unless you are building a simple 'App' from a 'Prog' with just one
-- module and you happen to already have the next valid 'ID' for that
-- module handy, you should probably use 'mkAppSafe' rather than this
-- function, as 'mkAppSafe' will always do the right thing for more
-- complicated 'Prog's and doesn't expose (as many) 'App'
-- implementation details to the caller. 'mkApp' is chiefly provided
-- for very simple initial programs and for testing purposes.
--
-- The value of the provided 'ID' should be at least one greater than
-- the largest 'ID' in any of the provided 'Prog''s 'progModules'.
-- (See 'nextProgID'.) The 'App' uses this initial 'ID' value to
-- guarantee that newly-created nodes in the program's AST are unique
-- across all editable modules in the 'Prog'. *Note*: 'mkApp' does not
-- enforce or otherwise check that this invariant holds! It is the
-- responsiblity of the caller.
--
-- Also N.B: the requirement that the provided 'ID' value should be
-- greater than the largest 'ID' is an implementation detail, and may
-- change in the future.
--
-- (Strictly speaking, the invariant on the provided 'ID' is
-- overconstrained, as the rest of our implementation depends only on
-- 'ID's being unique *per module*, and not across all editable
-- modules in the 'Prog' as 'App' requires. However, keeping track of
-- a per-module 'ID' would be much less ergonomic, and in practice
-- there's no pressure on the range of 'ID' values, so we can afford
-- to be a bit profligate.)
--
-- A valid value for the provided 'NameCounter' will depend on what
-- names already exist in the provided program, and is rather
-- implementation-dependent at the moment. In most cases, it should be
-- safe to use @toEnum 0@ as the initial value. We will make selecting
-- this value more foolproof, or eliminate it altogether, in the
-- future. See:
--
-- https://github.com/hackworthltd/primer/issues/510
mkApp :: ID -> NameCounter -> Prog -> App
mkApp i n p =
  let s = AppState i n p
   in App s s

-- | Given an 'App', return the next 'ID' that should be used to
-- create a new node.
appIdCounter :: App -> ID
appIdCounter = idCounter . currentState

-- | Given an 'App', return its 'NameCounter'.
appNameCounter :: App -> NameCounter
appNameCounter = nameCounter . currentState

-- | Given an 'App', return its 'Prog'.
appProg :: App -> Prog
appProg = prog . currentState

-- | Given an 'App', return its initial state.
appInit :: App -> App
appInit a =
  let s = initialState a
   in App s s

-- | Support for generating fresh IDs
instance Monad m => MonadFresh ID (EditAppM m e) where
  fresh = do
    id_ <- gets appIdCounter
    modify (\s -> s & _currentState % _idCounter .~ id_ + 1)
    pure id_

-- | Support for generating names. Basically just a counter so we don't
-- generate the same automatic name twice.
instance Monad m => MonadFresh NameCounter (EditAppM m e) where
  fresh = do
    nc <- gets appNameCounter
    modify (\s -> s & _currentState % _nameCounter .~ succ nc)
    pure nc

copyPasteSig :: MonadEdit m ProgError => Prog -> (GVarName, ID) -> GVarName -> [Action] -> m Prog
copyPasteSig p (fromDefName, fromTyId) toDefName setup = do
  c' <- focusNodeImports p fromDefName fromTyId
  c <- case c' of
    Left (Left _) -> throwError $ CopyPasteError "tried to copy-paste an expression into a signature"
    Left (Right zt) -> pure $ Left zt
    Right zt -> pure $ Right zt
  let smartHoles = progSmartHoles p
  finalProg <- editModuleOf (Just toDefName) p $ \mod toDefBaseName oldDef -> do
    let otherModules = filter ((/= moduleName mod) . moduleName) (progModules p)
    -- We intentionally throw away any changes in doneSetup other than via 'tgt'
    -- as these could be in other definitions referencing this one, due to
    -- types changing. However, we are going to do a full tc pass anyway,
    -- which will pick up any problems. It is better to do it in one batch,
    -- in case the intermediate state after 'setup' causes more problems
    -- than the final state does.
    doneSetup <- applyActionsToTypeSig smartHoles [] (mod, otherModules) (toDefBaseName, oldDef) setup
    tgt <- case doneSetup of
      Left err -> throwError $ ActionError err
      Right (_, tgt) -> pure $ focusOnlyType tgt
    let cTgt = either target target c
    let cScoped = cTgt
    freshCopy <- regenerateTypeIDs cScoped
    pasted <- case target tgt of
      TEmptyHole _ -> pure $ replace freshCopy tgt
      _ -> throwError $ CopyPasteError "copy/paste setup didn't select an empty hole"
    let newDef = oldDef{astDefType = fromZipper pasted}
    let newSel = NodeSelection SigNode (pasted ^. _target % _typeMetaLens % re _Right)
    pure (insertDef mod toDefBaseName (DefAST newDef), Just (Selection toDefName $ Just newSel))
  liftError ActionError $ tcWholeProg finalProg

-- | Checks every term and type definition in the editable modules.
-- Does not check imported modules.
tcWholeProg ::
  forall m e.
  ( MonadFresh ID m
  , MonadFresh NameCounter m
  , MonadNestedError TypeError e (ReaderT Cxt m)
  ) =>
  Prog ->
  m Prog
tcWholeProg p = do
  mods' <-
    checkEverything
      (progSmartHoles p)
      CheckEverything
        { trusted = []
        , toCheck = progModules p
        }
  let p' = p{progModules = mods'}
  -- We need to update the metadata cached in the selection
  let oldSel = progSelection p
  newSel <- case oldSel of
    Nothing -> pure Nothing
    Just s -> do
      let defName_ = s ^. _selectedDef
      updatedNode <- case s ^. _selectedNode of
        Nothing -> pure Nothing
        Just sel@NodeSelection{nodeType} -> do
          n <- runExceptT $ focusNode p' defName_ $ getID sel
          case (nodeType, n) of
            (BodyNode, Right (Left x)) -> pure $ Just $ NodeSelection BodyNode $ bimap (view _exprMetaLens . target) (view _typeMetaLens . target) x
            (SigNode, Right (Right x)) -> pure $ Just $ NodeSelection SigNode $ x ^. _target % _typeMetaLens % re _Right
            _ -> pure Nothing -- something's gone wrong: expected a SigNode, but found it in the body, or vv, or just not found it
      pure $
        Just $
          Selection
            { selectedDef = defName_
            , selectedNode = updatedNode
            }
  pure $ p'{progSelection = newSel}

-- | Do a full check of a 'Prog', both the imports and the local modules
tcWholeProgWithImports ::
  ( MonadFresh ID m
  , MonadFresh NameCounter m
  , MonadNestedError TypeError e (ReaderT Cxt m)
  ) =>
  Prog ->
  m Prog
tcWholeProgWithImports = tcWholeProg

copyPasteBody :: MonadEdit m ProgError => Prog -> (GVarName, ID) -> GVarName -> [Action] -> m Prog
copyPasteBody p (fromDefName, fromId) toDefName setup = do
  src' <- focusNodeImports p fromDefName fromId
  -- reassociate so get Expr+(Type+Type), rather than (Expr+Type)+Type
  let src = case src' of
        Left (Left e) -> Left e
        Left (Right t) -> Right (Left t)
        Right t -> Right (Right t)
  let smartHoles = progSmartHoles p
  finalProg <- editModuleOf (Just toDefName) p $ \mod toDefBaseName oldDef -> do
    -- The Loc zipper captures all the changes, they are only reflected in the
    -- returned Def, which we thus ignore
    doneSetup <- applyActionsToBody smartHoles (progAllModules p) oldDef setup
    tgt <- case doneSetup of
      Left err -> throwError $ ActionError err
      Right (_, tgt) -> pure tgt
    case (src, tgt) of
      (_, InBind _) -> throwError $ CopyPasteError "tried to paste an expression into a binder"
      (Left _, InType _) -> throwError $ CopyPasteError "tried to paste an expression into a type"
      (Right _, InExpr _) -> throwError $ CopyPasteError "tried to paste a type into an expression"
      (Right srcT, InType tgtT) -> do
        let srcSubtree = either target target srcT
        let scopedCopy = srcSubtree
        freshCopy <- regenerateTypeIDs scopedCopy
        pasted <- case target tgtT of
          TEmptyHole _ -> pure $ replace freshCopy tgtT
          _ -> throwError $ CopyPasteError "copy/paste setup didn't select an empty hole"
        let newDef = oldDef{astDefExpr = unfocusExpr $ unfocusType pasted}
        let newSel = NodeSelection BodyNode (pasted ^. _target % _typeMetaLens % re _Right)
        pure (insertDef mod toDefBaseName (DefAST newDef), Just (Selection toDefName $ Just newSel))
      (Left srcE, InExpr tgtE) -> do
        let scopedCopy = target srcE
        freshCopy <- regenerateExprIDs scopedCopy
        -- TODO: need to care about types and directions here (and write tests for this caring!)
        {-
        - Currently, with smart holes, nothing will go too wrong (i.e. no crashes/rejections happen), but if
        - smartholes were turned off (which currently needs changing in the source code, then things could go wrong, and the TC throws errors.
        - The cases we need to consider are (note that the metadata gives us what type each subtree was chk/syn (could be Nothing, due to our
        - represention, but we can consider that as a hole)
        - NB: as we always paste into a hole, it will always synth ?, but it may also have been checked against a concrete type
        - From    To    Want
        - e ∈ T   ∈ ?   e       if T is ? (or maybe don't special-case this, for consistency?)
        -               {? e ?} otherwise, to avoid "jumpy holes" (paste a 2∈Nat into '? True', would get '2 {? True ?}', but want '{? 2 ?} True', probably?
        - T ∋ t   ∈ ?   t : ?       if T is ? (or maybe don't special-case this, for consistency?)
        -               {? t : T ?} otherwise (avoid jumpy holes, as above)
        - e ∈ T   R ∋   e       if this would TC (i.e. T and R are consistent)
        -               {? e ?} otherwise
        - T ∋ t   R ∋   t           if this would TC (i.e. if T is more specific than R, I expect)
        -               {? t : T ?} otherwise
        -
        - Let's also tabulate what smartholes would give
        -    From    To    Want                   SH gives               Example ('raise' the term in >e<)
        -    e ∈ T   ∈ ?   e       if T is ?      e
        -!!!               {? e ?} otherwise      e, and jumpy holes.    (? : ? -> Bool -> ?) >Succ< True
        -    T ∋ t   ∈ ?   t : ?       if T is ?  t : ?                  ? >λx.?< ?
        -!!!               {? t : T ?} otherwise  t : ?                  (? : (Bool -> Bool) -> ?) >λx.?< ?
        -    e ∈ T   R ∋   e       if  would TC   e                      Bool ∋ not >not True< [using extra not so obv. syn, even if ctors are chk only]
        -                  {? e ?} otherwise      {? e ?}                Bool ∋ >isEven< ?
        -    T ∋ t   R ∋   t         if would TC  t                      Bool -> Bool ∋ (? : (Bool -> Bool) -> ?) >(λx.?)<
        -!!!               {? t : T ?} otherwise  {? t : ? ?}            Bool ∋ (? : (Bool -> Bool) -> ?) >(λx.?)<
        -
        - We could also consider what to do with embeddings: R ∋ e ∈ T: what happens for
        -     Bool ∋ even >(add 0 0)<   [use add so obv. syn, even if ctors are chk only]
        - ?
        -
        - so with SH, we are almost doing well, except we have a case of jumpy holes, and some cases of losing type information,
        - denoted by !!! above
        -}
        pasted <- case target tgtE of
          EmptyHole _ -> pure $ replace freshCopy tgtE
          _ -> throwError $ CopyPasteError "copy/paste setup didn't select an empty hole"
        let newDef = oldDef{astDefExpr = unfocusExpr pasted}
        let newSel = NodeSelection BodyNode (pasted ^. _target % _exprMetaLens % re _Left)
        pure (insertDef mod toDefBaseName (DefAST newDef), Just (Selection toDefName $ Just newSel))
  liftError ActionError $ tcWholeProg finalProg

lookupASTDef :: GVarName -> DefMap -> Maybe ASTDef
lookupASTDef name = defAST <=< Map.lookup name

-- | Run a computation in some context whose errors can be promoted to `ProgError`.
liftError :: MonadError ProgError m => (e -> ProgError) -> ExceptT e m b -> m b
liftError f = runExceptT >=> either (throwError . f) pure
