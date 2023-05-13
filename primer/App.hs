-- This module defines the high level application functions.

module App (
  module Available,
  App,
  mkApp,
  appProg,
  EditAppM,
  runEditAppM,
  Prog (..),
  progAllDefs,
  ProgError (..),
  handleEditRequest,
) where

import Foreword hiding (mod)

import Action (
  ProgAction (..),
  applyActionsToBody,
  applyActionsToTypeSig,
 )
import Available (
  NodeType (..),
 )
import Core (
  HasID (_id),
  _exprMetaLens,
  _typeMetaLens,
 )
import Data.Data (Data)
import Data.Map.Strict qualified as Map
import Def (
  ASTDef (..),
  Def (..),
 )
import DefUtils (globalInUse)
import Fresh (MonadFresh (..))
import Module (
  Module (moduleDefs),
  deleteDef,
  insertDef,
 )
import Optics (
  Setter',
  lens,
  sets,
  view,
  (.~),
  (?~),
 )
import Errors (ProgError (..))
import Zipper (
  locToEither,
  target,
 )

-- | The full program state.
data Prog = Prog
  { progModule :: Module
  -- ^ The editable "home" modules
  , progSelection :: Maybe Selection
  }
  deriving stock (Eq, Show, Read)

_progSelection :: Setter' Prog (Maybe Selection)
_progSelection = sets $ \f p -> p{progSelection = f $ progSelection p}

progAllDefs :: Prog -> Map Text Def
progAllDefs = moduleDefs . progModule

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

-- | Describes what interface element the user has selected.
-- A definition in the left hand nav bar, and possibly a node in that definition.
data Selection = Selection
  { selectedDef :: Text
  -- ^ the ID of some ASTDef
  , selectedNode :: Maybe NodeSelection
  }
  deriving stock (Eq, Show, Read)

-- | A selected node, in the body or type signature of some definition.
-- We have the following invariant: @nodeType = SigNode ==> isRight meta@
data NodeSelection = NodeSelection
  { nodeType :: NodeType
  , meta :: Int
  }
  deriving stock (Eq, Show, Read, Data)

instance HasID NodeSelection where
  _id = lens meta $ \ns i -> ns {meta = i}

-- * Request handlers

handleEditRequest :: forall m l. MonadEditApp l ProgError m => [ProgAction] -> m Prog
handleEditRequest actions = do
  (prog, _) <- gets appProg >>= \p -> foldlM go (p, Nothing) actions
  modify (\s -> s & _prog .~ prog)
  pure prog
  where
    go :: (Prog, Maybe Text) -> ProgAction -> m (Prog, Maybe Text)
    go (prog, mdef) a =
      applyProgAction prog mdef a <&> \prog' ->
        (prog', selectedDef <$> progSelection prog')

-- | Handle a 'ProgAction'
-- The 'GVarName' argument is the currently-selected definition, which is
-- provided for convenience: it is the same as the one in the progSelection.
applyProgAction :: MonadEdit m ProgError => Prog -> Maybe Text -> ProgAction -> m Prog
applyProgAction prog mdefName = \case
  MoveToDef d -> do
    let m = progModule prog
    case Map.lookup d $ moduleDefs m of
      Nothing -> throwError $ DefNotFound d
      Just _ -> pure $ prog & _progSelection ?~ Selection d Nothing
  DeleteDef d -> editModule prog $ \m ->
    case deleteDef m d of
      mod' -> do
        when (globalInUse d $ moduleDefs mod') $
          throwError $
            DefInUse d
        pure (mod', Nothing)
  RenameDef d newName -> editModuleOf (Just d) prog $ \m defName def -> do
    let defs = moduleDefs m
    if Map.member newName defs
      then throwError $ DefAlreadyExists newName
      else do
        let m' = m{moduleDefs = Map.insert newName (DefAST def) $ Map.delete defName defs}
        pure (m', Just $ Selection newName Nothing)
  BodyAction actions -> editModuleOf mdefName prog $ \m defName def -> do
    res <- applyActionsToBody (progModule prog) def actions
    case res of
      Left err -> throwError $ ActionError err
      Right (def', z) -> do
        let meta = either (view _exprMetaLens . target) (view _typeMetaLens . target) $ locToEither z
        pure
          ( insertDef m defName (DefAST def')
          , Just $
              Selection defName $
                Just
                  NodeSelection
                    { nodeType = BodyNode
                    , meta
                    }
          )
  SigAction actions -> editModuleOf mdefName prog $ \curMod defName def -> do
    res <- applyActionsToTypeSig curMod (defName, def) actions
    case res of
      Left err -> throwError $ ActionError err
      Right (mod', zt) -> do
        let node = target zt
            meta = view _typeMetaLens node
         in pure
              ( mod'
              , Just $
                  Selection defName $
                    Just
                      NodeSelection
                        { nodeType = SigNode
                        , meta = meta
                        }
              )

editModule ::
  MonadError ProgError m =>
  Prog ->
  (Module -> m (Module, Maybe Selection)) ->
  m Prog
editModule p f = do
  (m', s) <- f $ progModule p
  pure $
    p
      { progModule = m'
      , progSelection = s
      }

editModuleOf ::
  MonadError ProgError m =>
  Maybe Text ->
  Prog ->
  (Module -> Text -> ASTDef -> m (Module, Maybe Selection)) ->
  m Prog
editModuleOf mdefName prog f = case mdefName of
  Nothing -> throwError NoDefSelected
  Just defname -> editModule prog $ \m ->
    case Map.lookup defname (moduleDefs m) of
      Just (DefAST def) -> f m defname def
      _ -> throwError $ DefNotFound defname

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
type MonadEdit m e = (MonadFresh Int m, MonadError e m)

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
  { idCounter :: Int
  , prog :: Prog
  }
  deriving stock (Eq, Show, Read)

_idCounter :: Setter' App Int
_idCounter = sets $ \f as -> as{idCounter = f $ idCounter as}

_prog :: Setter' App Prog
_prog = sets $ \f as -> as{prog = f $ prog as}

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
mkApp :: Int -> Prog -> App
mkApp = App

-- | Given an 'App', return the next 'ID' that should be used to
-- create a new node.
appIdCounter :: App -> Int
appIdCounter = idCounter

-- | Given an 'App', return its 'Prog'.
appProg :: App -> Prog
appProg = prog

-- | Support for generating fresh IDs
instance Monad m => MonadFresh Int (EditAppM m e) where
  fresh = do
    id_ <- gets appIdCounter
    modify (\s -> s & _idCounter .~ id_ + 1)
    pure id_
