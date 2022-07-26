{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | The Primer API.
--
-- This module defines the Primer API, which is collection of
-- methods for creating, editing, and evaluating Primer programs.
--
-- While this Primer implementation *may* work for multiple students
-- concurrently editing the same program, at the moment we only
-- guarantee that it works for a single student per program (or
-- "session"). This implementation *is* safe for concurrent requests
-- to different programs, because all API requests are executed
-- transactionally.
module Primer.API (
  Env (..),
  PrimerM (..),
  runPrimerM,
  PrimerIO,
  runPrimerIO,
  PrimerErr (..),
  newSession,
  addSession,
  copySession,
  listSessions,
  getVersion,
  Tree,
  NodeBody,
  NodeStyle,
  Prog,
  Module,
  Def,
  getProgram,
  getSessionName,
  renameSession,
  edit,
  variablesInScope,
  generateNames,
  evalStep,
  evalFull,
  flushSessions,
  -- The following are exported only for testing.
  viewTreeType,
  viewTreeExpr,
  getApp,
  test,
) where

import Foreword

import Control.Concurrent.STM (
  STM,
  TBQueue,
  atomically,
  newEmptyTMVar,
  takeTMVar,
  writeTBQueue,
 )
import Control.Monad.Cont (MonadCont)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Trans (MonadTrans)
import Control.Monad.Writer (MonadWriter)
import Control.Monad.Zip (MonadZip)
import Data.Aeson (ToJSON)
import Data.Data (showConstr, toConstr)
import Data.Generics.Uniplate.Data qualified as U
import Data.List (foldr1)
import Data.List.Extra (dropEnd)
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Text qualified as T
import Data.Tuple.Extra (fst3)
import ListT qualified (toList)
import Optics ((^.))
import Primer.App (
  App,
  EditAppM,
  EvalFullReq (..),
  EvalFullResp (..),
  EvalReq (..),
  EvalResp (..),
  MutationRequest,
  ProgError,
  QueryAppM,
  Question (..),
  handleEvalFullRequest,
  handleEvalRequest,
  handleGetProgramRequest,
  handleMutationRequest,
  handleQuestion,
  newApp,
  progImports,
  progModules,
  runEditAppM,
  runQueryAppM,
 )
import Primer.App qualified as App
import Primer.Core (
  ASTDef (..),
  Bind' (..),
  CaseBranch' (CaseBranch),
  Expr,
  Expr' (..),
  ExprMeta,
  GVarName,
  GlobalName (..),
  HasID (..),
  ID,
  Kind,
  LVarName,
  ModuleName,
  PrimCon (..),
  TmVarRef (GlobalVarRef, LocalVarRef),
  TyConName,
  TyVarName,
  Type,
  Type' (TForall),
  TypeMeta,
  defAST,
  defType,
  getID,
  mkSimpleModuleName,
  moduleNamePretty,
  unLocalName,
 )
import Primer.Core.DSL (create')
import Primer.Database (
  OffsetLimit,
  Page,
  Session (Session),
  SessionData (..),
  SessionId,
  SessionName,
  Sessions,
  Version,
  defaultSessionName,
  fromSessionName,
  newSessionId,
  pageList,
  safeMkSessionName,
 )
import Primer.Database qualified as Database (
  Op (
    Insert,
    ListSessions,
    LoadSession,
    UpdateApp,
    UpdateName
  ),
  OpStatus (
    Failure,
    Success
  ),
 )
import Primer.Examples (even3Prog)
import Primer.Examples qualified as Examples
import Primer.Module (moduleDefs, moduleDefsQualified, moduleName, moduleTypesQualified)
import Primer.Name (Name, unName)
import StmContainers.Map qualified as StmMap
import System.IO.Unsafe (unsafePerformIO)
import System.Process.Extra (readCreateProcess, readProcess, shell, system)
import Prelude (read)

-- | The API environment.
data Env = Env
  { sessions :: Sessions
  , dbOpQueue :: TBQueue Database.Op
  , version :: Version
  }

-- | The Primer API monad transformer.
newtype PrimerM m a = PrimerM {unPrimerM :: ReaderT Env m a}
  deriving
    ( Functor
    , Applicative
    , Alternative
    , Monad
    , MonadError e
    , MonadThrow
    , MonadCatch
    , MonadMask
    , MonadReader Env
    , MonadIO
    , MonadFail
    , MonadFix
    , MonadPlus
    , MonadTrans
    , MonadState s
    , MonadWriter w
    , MonadZip
    , MonadCont
    )

-- | Run a 'PrimerM' action with the given 'Env'.
runPrimerM :: PrimerM m a -> Env -> m a
runPrimerM = runReaderT . unPrimerM

-- | The Primer API monad transformer applied to IO.
type PrimerIO = PrimerM IO

-- | Run a 'PrimerIO' action with the given 'Env'.
runPrimerIO :: PrimerIO a -> Env -> IO a
runPrimerIO = runPrimerM

{- HLINT ignore PrimerErr "Use newtype instead of data" -}

-- | Primer exception class.
data PrimerErr = DatabaseErr Text deriving (Show)

instance Exception PrimerErr

sessionsTransaction :: (MonadIO m) => (Sessions -> TBQueue Database.Op -> STM a) -> PrimerM m a
sessionsTransaction f = do
  ss <- asks sessions
  q <- asks dbOpQueue
  liftIO $ atomically $ f ss q

data SessionOp a where
  EditApp :: (App -> (a, App)) -> SessionOp a
  QueryApp :: (App -> a) -> SessionOp a
  GetSessionName :: SessionOp Text
  GetSessionData :: SessionOp SessionData
  RenameSession :: Text -> SessionOp Text

-- A note about the implementation here. When the session is missing
-- from the in-memory database, we can't queue the database request to
-- load the session *and* wait for the database thread's asynchronous
-- reply in the same STM transaction. That's because the wait will
-- immediately block until the reply has been received, preventing the
-- transaction from completing, but the database request won't
-- actually be sent until the transaction is complete. This would
-- cause a deadlock!
withSession' :: (MonadIO m, MonadThrow m) => SessionId -> SessionOp a -> PrimerM m a
withSession' sid op = do
  hndl <- sessionsTransaction $ \ss q -> do
    query <- StmMap.lookup sid ss
    case query of
      Nothing -> do
        -- The session is not in memory. Ask the database thread to
        -- load the missing session, and return the "callback"
        -- 'TMVar'.
        --
        -- Note: see above for why we don't immediately wait
        -- for the callback.
        callback <- newEmptyTMVar
        writeTBQueue q $ Database.LoadSession sid ss callback
        return $ Left callback
      Just s@(SessionData appl n) ->
        -- The session is in memory, let's do this.
        case op of
          EditApp f -> do
            let (res, appl') = f appl
            StmMap.insert (SessionData appl' n) sid ss
            writeTBQueue q $ Database.UpdateApp sid appl'
            pure $ Right res
          QueryApp f -> pure $ Right $ f appl
          GetSessionName -> pure $ Right (fromSessionName n)
          GetSessionData -> pure $ Right s
          RenameSession n' ->
            let newName = safeMkSessionName n'
             in do
                  StmMap.insert (SessionData appl newName) sid ss
                  writeTBQueue q $ Database.UpdateName sid newName
                  pure $ Right (fromSessionName newName)
  case hndl of
    Left callback -> do
      -- The session was missing from the in-memory database. Once we
      -- get here, we know we've made the database load request, so
      -- now we can wait for the callback.
      dbResult <- liftIO $ atomically $ takeTMVar callback
      case dbResult of
        Database.Failure msg ->
          -- The load failed for some reason.
          throwM $ DatabaseErr msg
        Database.Success ->
          -- The session has been loaded, try the operation again by
          -- recursing.
          --
          -- Note: this should be a tail call as long as we compile
          -- with -O (or at least -floopification), because it's a
          -- known function, it's in tail position, and it's fully
          -- saturated. Ref:
          -- https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/rts/haskell-execution/function-calls
          withSession' sid op
    Right result ->
      -- We performed the session transaction, now return the result.
      pure result

-- | Create a new session and return the session ID.
--
-- The session's initial program is 'newApp'.
newSession :: (MonadIO m) => PrimerM m SessionId
newSession = addSession' newApp defaultSessionName

-- | Given an 'App' and a proposed session name as 'Text', create a
-- new session with the given app and name, and return the session ID.
--
-- If the given session name is invalid, it will be replaced with a
-- default session name. However, no indication to the caller is given
-- when this occurs. Query the returned session ID to determine the
-- actual session name that was assigned.
--
-- Note: this API method is currently a special case, and we do not
-- expect typical API clients to use it, because it bypasses the edit
-- API by permitting the caller to directly insert an existing 'App'
-- into the database. The chief use case for this API method is to
-- insert pre-made programs built with the Primer Haskell DSL into a
-- new Primer database. Whether this method should be added to the
-- HTTP API is tracked here:
--
-- https://github.com/hackworthltd/primer/issues/550
addSession :: (MonadIO m) => App -> Text -> PrimerM m SessionId
addSession a n = addSession' a (safeMkSessionName n)

addSession' :: (MonadIO m) => App -> SessionName -> PrimerM m SessionId
addSession' a n = do
  nextSID <- liftIO newSessionId
  sessionsTransaction $ \ss q -> do
    StmMap.insert (SessionData a n) nextSID ss
    writeTBQueue q $ Database.Insert nextSID a n
    pure nextSID

-- | Copy the given session to a new session, and return the new
-- session's ID.
--
-- We implement this as 2 separate transactions: 1 to retrieve the
-- source session, and 1 to insert the copy. Semantically, this is
-- fine, and it should be more fair on a busy system than a single
-- transaction which takes longer.
copySession :: (MonadIO m, MonadThrow m) => SessionId -> PrimerM m SessionId
copySession srcId = do
  copy <- withSession' srcId GetSessionData
  nextSID <- liftIO newSessionId
  sessionsTransaction $ \ss q -> do
    StmMap.insert copy nextSID ss
    writeTBQueue q $ Database.Insert nextSID (sessionApp copy) (sessionName copy)
    pure nextSID

-- If the input is 'False', return all sessions in the database;
-- otherwise, only the in-memory sessions.
--
-- Currently the pagination support is "extract the whole list from the DB,
-- then select a portion". This should be improved to only extract the
-- appropriate section from the DB in the first place.
listSessions :: (MonadIO m) => Bool -> OffsetLimit -> PrimerM m (Page Session)
listSessions False ol = do
  q <- asks dbOpQueue
  callback <- liftIO $
    atomically $ do
      cb <- newEmptyTMVar
      writeTBQueue q $ Database.ListSessions ol cb
      return cb
  liftIO $ atomically $ takeTMVar callback
listSessions _ ol = sessionsTransaction $ \ss _ -> do
  kvs' <- ListT.toList $ StmMap.listT ss
  let kvs = uncurry Session . second sessionName <$> kvs'
  pure $ pageList ol kvs

getVersion :: (Monad m) => PrimerM m Version
getVersion = asks version

getSessionName :: (MonadIO m, MonadThrow m) => SessionId -> PrimerM m Text
getSessionName sid = withSession' sid GetSessionName

renameSession :: (MonadIO m, MonadThrow m) => SessionId -> Text -> PrimerM m Text
renameSession sid n = withSession' sid $ RenameSession n

-- Run an 'EditAppM' action, using the given session ID to look up and
-- pass in the app state for that session.
liftEditAppM :: (MonadIO m, MonadThrow m) => EditAppM a -> SessionId -> PrimerM m (Either ProgError a)
liftEditAppM h sid = withSession' sid (EditApp $ runEditAppM h)

-- Run a 'QueryAppM' action, using the given session ID to look up and
-- pass in the app state for that session.
liftQueryAppM :: (MonadIO m, MonadThrow m) => QueryAppM a -> SessionId -> PrimerM m (Either ProgError a)
liftQueryAppM h sid = withSession' sid (QueryApp $ runQueryAppM h)

-- | Given a 'SessionId', return the session's 'App'.
--
-- Note: this API method is currently a special case, and we do not
-- expect typical API clients to use it. Its primary use is for
-- testing. Whether we should add it to the HTTP API is tracked here:
--
-- https://github.com/hackworthltd/primer/issues/550
getApp :: (MonadIO m, MonadThrow m) => SessionId -> PrimerM m App
getApp sid = withSession' sid $ QueryApp identity

getProgram :: (MonadIO m, MonadThrow m) => SessionId -> PrimerM m Prog
getProgram sid = withSession' sid $ QueryApp $ viewProg . handleGetProgramRequest

-- | A frontend will be mostly concerned with rendering, and does not need the
-- full complexity of our AST for that task. 'Tree' is a simplified view with
-- just enough information to render nicely.
data Tree = Tree
  { nodeId :: ID
  , ann :: Text
  , style :: NodeStyle
  -- ^ P, λ, etc
  , body :: NodeBody
  , childTrees :: [Tree]
  , rightChild :: Maybe Tree
  }
  deriving (Show, Eq, Generic)

instance ToJSON Tree

data NodeBody
  = TextBody Text
  | BoxBody Tree
  | NoBody
  deriving (Show, Eq, Generic)
instance ToJSON NodeBody

data NodeStyle
  = StyleHole
  | StyleEmptyHole
  | StyleAnn
  | StyleApp
  | StyleAPP
  | StyleCon
  | StyleLam
  | StyleLAM
  | StyleVar
  | StyleGlobalVarRef
  | StyleLocalVarRef
  | StyleLet
  | StyleLetType
  | StyleLetrec
  | StyleCase
  | StylePrimCon
  deriving (Show, Eq, Generic)
instance ToJSON NodeStyle

-- | This type is the API's view of a 'App.Prog'
-- (this is expected to evolve as we flesh out the API)
newtype Prog = Prog
  { modules :: [Module]
  }
  deriving (Generic)

instance ToJSON Prog

-- | This type is the API's view of a 'Module.Module'
-- (this is expected to evolve as we flesh out the API)
data Module = Module
  { modname :: ModuleName
  , editable :: Bool
  , types :: [TyConName]
  , -- We don't use Map Name Def as it is rather redundant since each
    -- Def carries a name field, and it is difficult to enforce that
    -- "the keys of this object match the name field of the
    -- corresponding value".
    defs :: [Def]
  }
  deriving (Generic)

instance ToJSON Module

-- | This type is the api's view of a 'Primer.Core.Def'
-- (this is expected to evolve as we flesh out the API)
data Def = Def
  { name :: GVarName
  , type_ :: Tree
  , term :: Maybe Tree
  -- ^ definitions with no associated tree are primitives
  }
  deriving (Generic)

instance ToJSON Def

viewProg :: App.Prog -> Prog
viewProg p =
  Prog{modules = map (viewModule True) (progModules p) <> map (viewModule False) (progImports p)}
  where
    viewModule e m =
      Module
        { modname = moduleName m
        , editable = e
        , types = fst <$> Map.assocs (moduleTypesQualified m)
        , defs =
            ( \(n, d) ->
                Def
                  { name = n
                  , type_ = viewTreeType $ defType d
                  , term = viewTreeExpr . astDefExpr <$> defAST d
                  }
            )
              <$> Map.assocs (moduleDefsQualified m)
        }

-- | A simple method to extract 'Tree's from 'Expr's. This is injective.
viewTreeExpr :: Expr -> Tree
viewTreeExpr e0 = case e0 of
  Hole _ e ->
    Tree
      { nodeId
      , ann = "{?}"
      , style = StyleHole
      , body = NoBody
      , childTrees = [viewTreeExpr e]
      , rightChild = Nothing
      }
  EmptyHole _ ->
    Tree
      { nodeId
      , ann = "?"
      , style = StyleEmptyHole
      , body = NoBody
      , childTrees = []
      , rightChild = Nothing
      }
  Ann _ e t ->
    Tree
      { nodeId
      , ann = "Ann"
      , style = StyleAnn
      , body = NoBody
      , childTrees = [viewTreeExpr e, viewTreeType t]
      , rightChild = Nothing
      }
  App _ e1 e2 ->
    Tree
      { nodeId
      , ann = "$"
      , style = StyleApp
      , body = NoBody
      , childTrees = [viewTreeExpr e1, viewTreeExpr e2]
      , rightChild = Nothing
      }
  APP _ e t ->
    Tree
      { nodeId
      , ann = "@"
      , style = StyleAPP
      , body = NoBody
      , childTrees = [viewTreeExpr e, viewTreeType t]
      , rightChild = Nothing
      }
  Con _ s ->
    Tree
      { nodeId
      , ann = "V"
      , style = StyleCon
      , body = TextBody $ showGlobal s
      , childTrees = []
      , rightChild = Nothing
      }
  Lam _ s e ->
    Tree
      { nodeId
      , ann = "λ"
      , style = StyleLam
      , body = TextBody $ unName $ unLocalName s
      , childTrees = [viewTreeExpr e]
      , rightChild = Nothing
      }
  LAM _ s e ->
    Tree
      { nodeId
      , ann = "Λ"
      , style = StyleLAM
      , body = TextBody $ unName $ unLocalName s
      , childTrees = [viewTreeExpr e]
      , rightChild = Nothing
      }
  Var _ s ->
    Tree
      { nodeId
      , ann = "Var"
      , style = StyleVar
      , body = TextBody $ case s of
          GlobalVarRef n -> showGlobal n
          LocalVarRef n -> unName $ unLocalName n
      , childTrees = []
      , rightChild = Nothing
      }
  Let _ s e1 e2 ->
    Tree
      { nodeId
      , ann = "let"
      , style = StyleLet
      , body = TextBody $ unName $ unLocalName s
      , childTrees = [viewTreeExpr e1, viewTreeExpr e2]
      , rightChild = Nothing
      }
  LetType _ s t e ->
    Tree
      { nodeId
      , ann = "let type"
      , style = StyleLetType
      , body = TextBody $ unName $ unLocalName s
      , childTrees = [viewTreeExpr e, viewTreeType t]
      , rightChild = Nothing
      }
  Letrec _ s e1 t e2 ->
    Tree
      { nodeId
      , ann = "let rec"
      , style = StyleLetrec
      , body = TextBody $ unName $ unLocalName s
      , childTrees = [viewTreeExpr e1, viewTreeType t, viewTreeExpr e2]
      , rightChild = Nothing
      }
  Case _ e bs ->
    Tree
      { nodeId
      , ann = "match"
      , style = StyleCase
      , body = NoBody
      , childTrees = [viewTreeExpr e]
      , rightChild =
          foldr
            ( \(CaseBranch n binds eRes) next ->
                Just $
                  Tree
                    rand
                    "P"
                    StyleCase
                    ( BoxBody $
                        Tree
                          rand
                          "V"
                          StyleCon
                          (TextBody $ showGlobal n)
                          ( foldr
                              ( \(Bind m ln) next' ->
                                  pure $
                                    Tree
                                      (m ^. _id)
                                      "Var"
                                      StyleVar
                                      (TextBody $ unName $ unLocalName ln)
                                      next'
                                      Nothing
                              )
                              []
                              binds
                          )
                          Nothing
                    )
                    [viewTreeExpr eRes]
                    next
            )
            Nothing
            bs
      }
  PrimCon _ pc ->
    Tree
      { nodeId
      , ann = "V"
      , style = StylePrimCon
      , body = TextBody $ case pc of
          PrimChar c -> T.singleton c
          PrimInt c -> show c
      , childTrees = []
      , rightChild = Nothing
      }
  where
    nodeId = e0 ^. _id
    showGlobal n = moduleNamePretty (qualifiedModule n) <> "." <> unName (baseName n)

-- | Similar to 'viewTreeExpr', but for 'Type's
viewTreeType :: Type -> Tree
viewTreeType x = Tree (x ^. _id) "T" StylePrimCon (TextBody "unimplemented") [] Nothing

edit :: (MonadIO m, MonadThrow m) => SessionId -> MutationRequest -> PrimerM m (Either ProgError App.Prog)
edit sid req = liftEditAppM (handleMutationRequest req) sid

{-# NOINLINE rand #-}
rand :: ID
rand = fromIntegral . read @Int . dropEnd 1 $ unsafePerformIO (readCreateProcess (shell "echo $RANDOM") "")

variablesInScope ::
  (MonadIO m, MonadThrow m) =>
  SessionId ->
  (GVarName, ID) ->
  PrimerM m (Either ProgError (([(TyVarName, Kind)], [(LVarName, Type' ())]), [(GVarName, Type' ())]))
variablesInScope sid (defname, exprid) =
  liftQueryAppM (handleQuestion (VariablesInScope defname exprid)) sid

generateNames :: (MonadIO m, MonadThrow m) => SessionId -> ((GVarName, ID), Either (Maybe (Type' ())) (Maybe Kind)) -> PrimerM m (Either ProgError [Name])
generateNames sid ((defname, exprid), tk) =
  liftQueryAppM (handleQuestion $ GenerateName defname exprid tk) sid

evalStep :: (MonadIO m, MonadThrow m) => SessionId -> EvalReq -> PrimerM m (Either ProgError EvalResp)
evalStep sid req =
  liftEditAppM (handleEvalRequest req) sid

evalFull :: (MonadIO m, MonadThrow m) => SessionId -> EvalFullReq -> PrimerM m (Either ProgError EvalFullResp)
evalFull sid req =
  liftEditAppM (handleEvalFullRequest req) sid

flushSessions :: (MonadIO m) => PrimerM m ()
flushSessions = do
  sessionsTransaction $ \ss _ -> do
    StmMap.reset ss
  pure ()

pPrint :: Show a => a -> IO ()
pPrint = putStrLn <=< readProcess "pretty-simple" [] . show

test :: IO ()
test = do
  -- prettyPrintExpr e
  -- pPrint $ viewTreeExpr e
  -- pPrint $ void $ layoutSmart defaultLayoutOptions $ prettyExpr defaultPrettyOptions e
  -- pPrint $ layoutCompact @_ @() $ prettyExpr defaultPrettyOptions e
  -- pPrint $ treeForm $ layoutCompact @_ @() $ prettyExpr defaultPrettyOptions e
  -- pPrint $ treeForm $ layoutSmart defaultLayoutOptions $ prettyExpr defaultPrettyOptions e
  -- pPrint $ viewTreeExpr' e
  pPrint $ viewTreeExpr e
  where
    -- pPrint $ viewTreeExpr e

    -- e = astDefExpr . fromMaybe undefined . defAST . fromMaybe undefined . head . toList . moduleDefs . fromMaybe undefined . head . progModules $ fst3 even3Prog
    e = astDefExpr . fromMaybe witness . defAST . snd . create' $ Examples.not $ mkSimpleModuleName "Prelude"
