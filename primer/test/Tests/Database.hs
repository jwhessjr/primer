{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Tests.Database where

import Foreword

import Control.Concurrent.STM (
  atomically,
  isEmptyTBQueue,
  newEmptyTMVarIO,
  newTBQueueIO,
  writeTBQueue,
 )
import Control.Monad.Log (WithSeverity)
import Control.Monad.Trans (
  MonadTrans,
 )
import Control.Monad.Trans.Identity (
  IdentityT (..),
 )
import Data.Text qualified as Text
import Primer.API (
  Env (..),
  MonadAPILog,
  PrimerM,
  addSession,
  edit,
  renameSession,
  runPrimerM,
 )
import Primer.API qualified as API
import Primer.App (
  MutationRequest (Edit),
  ProgAction (CreateDef),
 )
import Primer.Core (
  mkSimpleModuleName,
 )
import Primer.Database (
  MonadDb (..),
  OffsetLimit (OL),
  Op (LoadSession),
  ServiceCfg (..),
  defaultSessionName,
  fromSessionName,
  mkSessionName,
  runNullDb,
  safeMkSessionName,
  serve,
 )
import Primer.Examples (
  even3App,
 )
import Primer.Log (PureLogT, runPureLogT)
import Primer.Test.Util (LogMsg, assertNoSevereLogs)
import StmContainers.Map qualified as StmMap
import Test.Tasty
import Test.Tasty.HUnit

test_unmodified :: TestTree
test_unmodified =
  testGroup
    "unmodified"
    [ testSessionName'
        "foobar"
        "foobar"
    , testSessionName'
        "foo bar"
        "foo bar"
    , testSessionName'
        "preserve punctuation"
        "... this is a session: it's mine!"
    , testSessionName'
        "preserve whitespace"
        "This    is a    \tsession"
    , testSessionName'
        "emoji"
        "😄😂🤣🤗 🦊 🦈"
    ]
  where
    testSessionName' testName t = testSessionName testName t t

test_modified :: TestTree
test_modified =
  testGroup
    "modified"
    [ testGroup
        "newline"
        [ testSessionName
            "start"
            "\nfoo bar"
            "foo bar"
        , testSessionName
            "middle"
            "foo\nbar"
            "foo"
        , testSessionName
            "end"
            "foo bar\n"
            "foo bar"
        , testSessionName
            "end, after space"
            "foo bar  \n"
            "foo bar"
        , testSessionName
            "start and middle"
            "\nfoo\nbar"
            "foo"
        , testSessionName
            "strip whitespace"
            "   \nfoo bar baz  \n  "
            "foo bar baz"
        ]
    , let tooLong = toS . concat $ replicate 7 ['0' .. '9']
       in testSessionName
            "truncate at 64"
            tooLong
            (Text.take 64 tooLong)
    ]

test_invalid :: TestTree
test_invalid =
  testGroup
    "invalid"
    [ testSessionName'
        "empty"
        ""
    , testSessionName'
        "all whitespace"
        " \t\n  \n"
    ]
  where
    testSessionName' testName t =
      testGroup
        testName
        [ testCase "unsafe" $ case mkSessionName t of
            Nothing -> pure ()
            Just _ -> assertFailure "name is valid"
        , testCase "safe" $
            safeMkSessionName t @?= defaultSessionName
        ]

insertTest :: (MonadIO m, MonadAPILog l m) => PrimerM m ()
insertTest = do
  void $ addSession "even3App" even3App

updateAppTest :: (MonadIO m, MonadThrow m, MonadAPILog l m) => PrimerM m ()
updateAppTest = do
  sid <- addSession "even3App" even3App
  void $ edit sid $ Edit [CreateDef (mkSimpleModuleName "Even3") $ Just "newDef"]

updateNameTest :: (MonadIO m, MonadThrow m, MonadAPILog l m) => PrimerM m ()
updateNameTest = do
  sid <- addSession "even3App" even3App
  void $ renameSession sid "even3App'"

loadSessionTest :: (MonadIO m, MonadAPILog l m) => PrimerM m ()
loadSessionTest = do
  sid <- addSession "even3App" even3App
  -- No easy way to do this from the API, so we do it here by hand.
  callback <- liftIO newEmptyTMVarIO
  q <- asks dbOpQueue
  ss <- asks sessions
  void $ liftIO $ atomically $ writeTBQueue q $ LoadSession sid ss callback

listSessionsTest :: (MonadIO m, MonadAPILog l m) => PrimerM m ()
listSessionsTest = do
  void $ addSession "even3App" even3App
  void $ API.listSessions True $ OL 0 $ Just 100

deleteSessionTest :: (MonadIO m, MonadThrow m, MonadAPILog l m) => PrimerM m ()
deleteSessionTest = do
  sid <- addSession "even3App" even3App
  void $ API.deleteSession sid

test_insert_empty_q :: TestTree
test_insert_empty_q = emptyQHarness "database Insert leaves an empty op queue" $ do
  insertTest

test_updateapp_empty_q :: TestTree
test_updateapp_empty_q = emptyQHarness "database UpdateApp leaves an empty op queue" $ do
  updateAppTest

test_updatename_empty_q :: TestTree
test_updatename_empty_q = emptyQHarness "database UpdateName leaves an empty op queue" $ do
  updateNameTest

test_loadsession_empty_q :: TestTree
test_loadsession_empty_q = emptyQHarness "database LoadSession leaves an empty op queue" $ do
  loadSessionTest

test_listsessions_empty_q :: TestTree
test_listsessions_empty_q = emptyQHarness "database ListSessions leaves an empty op queue" $ do
  listSessionsTest

test_deletesession_empty_q :: TestTree
test_deletesession_empty_q = emptyQHarness "database DeleteSession leaves an empty op queue" $ do
  deleteSessionTest

test_insert_faildb :: TestTree
test_insert_faildb = faildbHarness "database Insert leaves behind an op" $ do
  insertTest

test_updateapp_faildb :: TestTree
test_updateapp_faildb = faildbHarness "database UpdateApp leaves behind an op" $ do
  updateAppTest

test_updatename_faildb :: TestTree
test_updatename_faildb = faildbHarness "database UpdateName leaves behind an op" $ do
  updateNameTest

test_loadsession_faildb :: TestTree
test_loadsession_faildb = faildbHarness "database LoadSession leaves behind an op" $ do
  loadSessionTest

test_listsessions_faildb :: TestTree
test_listsessions_faildb = faildbHarness "database ListSessions leaves behind an op" $ do
  listSessionsTest

test_deletesession_faildb :: TestTree
test_deletesession_faildb = faildbHarness "database DeleteSession leaves behind an op" $ do
  deleteSessionTest

testSessionName :: TestName -> Text -> Text -> TestTree
testSessionName testName t expected =
  testGroup
    testName
    [ testCase "unsafe" $ case mkSessionName t of
        Nothing -> assertFailure "name is invalid"
        Just sn -> fromSessionName sn @?= expected
    , testCase "safe" $
        fromSessionName (safeMkSessionName t) @?= expected
    ]
emptyQHarness :: Text -> PrimerM (PureLogT (WithSeverity LogMsg) IO) () -> TestTree
emptyQHarness desc test = testCaseSteps (toS desc) $ \step' -> do
  dbOpQueue <- newTBQueueIO 4
  inMemorySessions <- StmMap.newIO
  dbSessions <- StmMap.newIO
  let version = "git123"
  nullDbProc <- async $ runNullDb dbSessions $ serve $ ServiceCfg dbOpQueue version
  testProc <- async $ runPureLogT $ flip runPrimerM (Env inMemorySessions dbOpQueue version) $ do
    test
    -- Give 'nullDbProc' time to empty the queue.
    liftIO $ threadDelay 100000
  waitEitherCatchCancel nullDbProc testProc >>= \case
    Left (Left e) -> assertFailure $ "nullDbProc threw an exception: " <> show e
    Left (Right v) -> absurd v
    Right (Left e) -> assertFailure $ "testProc threw an exception: " <> show e
    Right (Right (_, logs)) -> do
      step' "Check that the database op queue is empty"
      qempty <- liftIO $ atomically $ isEmptyTBQueue dbOpQueue
      assertBool "Queue should be empty" qempty
      assertNoSevereLogs logs

-- | A "fail" database that fails on every operation.
newtype FailDbT m a = FailDbT {unFailDbT :: IdentityT m a}
  deriving newtype
    ( Functor
    , Applicative
    , Alternative
    , Monad
    , MonadError e
    , MonadThrow
    , MonadCatch
    , MonadMask
    , MonadIO
    , MonadFail
    , MonadPlus
    , MonadTrans
    )

-- | The 'FailDbT' monad transformer applied to 'IO'.
type FailDb a = FailDbT IO a

-- | A simple 'Exception' type for 'FailDb' computations.
newtype FailDbException = FailDbException Text
  deriving stock (Eq, Show)

instance Exception FailDbException

instance (MonadThrow m) => MonadDb (FailDbT m) where
  insertSession _ _ _ _ _ = throwM $ FailDbException "insertSession"
  updateSessionApp _ _ _ _ = throwM $ FailDbException "updateSessionApp"
  updateSessionName _ _ _ _ = throwM $ FailDbException "updateSessionName"
  listSessions _ = throwM $ FailDbException "listSessions"
  querySessionId _ = throwM $ FailDbException "querySessionId"
  deleteSession _ = throwM $ FailDbException "deleteSession"

-- | Run a 'FailDbT' action in a transformer stack.
runFailDbT :: FailDbT m a -> m a
runFailDbT m = runIdentityT $ unFailDbT m

-- | Run a 'FailDb' action in 'IO'.
runFailDb :: FailDb a -> IO a
runFailDb = runFailDbT

faildbHarness :: Text -> PrimerM (PureLogT (WithSeverity LogMsg) IO) () -> TestTree
faildbHarness desc test = testCaseSteps (toS desc) $ \step' -> do
  dbOpQueue <- newTBQueueIO 4
  inMemorySessions <- StmMap.newIO
  let version = "git123"
  failDbProc <- async $ runFailDb $ serve $ ServiceCfg dbOpQueue version
  testProc <- async $ runPureLogT $ flip runPrimerM (Env inMemorySessions dbOpQueue version) $ do
    test
    -- Give 'failDbProc' time to throw.
    liftIO $ threadDelay 100000
  waitEitherCatchCancel failDbProc testProc >>= \case
    Left (Left _) -> do
      step' "Check that the database op queue is non-empty"
      qempty <- liftIO $ atomically $ isEmptyTBQueue dbOpQueue
      assertBool "Queue should not be empty" (not qempty)
    Left (Right v) -> absurd v
    Right (Left e) -> assertFailure $ "testProc threw an exception: " <> show e
    Right (Right _) -> assertFailure "failDbProc should have thrown an exception, but it didn't (hint: we might need to increase the threadDelay)"
