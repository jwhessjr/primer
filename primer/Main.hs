module Main (main) where

import Foreword

import Action (
  ActionError (NameCapture),
  toProgActionInput,
  toProgActionNoInput,
 )
import App (
  App,
  EditAppM,
  NodeType (..),
  Prog (..),
  ProgError (ActionError, DefAlreadyExists),
  appProg,
  handleEditRequest,
  mkApp,
  progAllDefs,
  progModule,
  runEditAppM,
 )
import Available qualified as Available
import Core (
  GVarName,
  ID,
 )
import CoreUtils (
  exprIDs,
  typeIDs,
 )
import DSL
import Data.List.Extra (enumerate)
import Data.Map qualified as Map
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Def (
  ASTDef (..),
  Def (DefAST),
  defAST,
 )
import Fresh (MonadFresh)
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Internal.Property (
  Property (propertyConfig, propertyTest),
  ShrinkPath,
  Skip (SkipToShrink),
  TestCount,
 )
import Hedgehog.Internal.Report (
  FailedAnnotation, -- (FailedAnnotation)
  FailureReport (failureAnnotations, failureShrinkPath),
  Report (reportSeed, reportTests),
  Result (..),
  reportStatus,
 )
import Hedgehog.Internal.Runner (checkReport)
import Hedgehog.Internal.Seed qualified as Seed
import Hedgehog.Range qualified as Range
import Module (
  Module (..),
 )
import Numeric.Natural (Natural)
import Optics (toListOf)
import TypeDef (ASTTypeDef (..), TypeDef (..))

main :: IO ()
main = do
  let n = 100
  rs <- replicateM (fromIntegral n) runAndRecheck
  let cs = count rs
  void $ M.traverseWithKey (\ri c -> putStrLn $ showPad ri <> " : " <> show c) cs
  if (cs M.! RecheckPass) + (cs M.! RecheckDefeat) > n `div` 2
    then putStrLn @Text "This tickled non-replay bug > 50%"
    else die "This did not tickle non-replay bug much"

-- Bounded & Enum: we explicitly give counts of 0 for those which did not appear
count :: (Bounded a, Enum a, Ord a) => [a] -> Map a Natural
count as = M.unionsWith (+) $ M.fromList [(a, 0) | a <- enumerate] : fmap (`M.singleton` 1) as

-- This runs the test once with a random seed, and
-- - if it fails then rechecks it with the reported skip/shrink, reporting whether it finds an error again
-- - if it passes or gives up, report that
runAndRecheck :: IO RRInfo
runAndRecheck = either identity absurd <$> runExceptT go
  where
    go :: ExceptT RRInfo IO Void
    go = do
      seed <- Seed.random
      (shrink, _failann) <-
        ExceptT $
          runProp seed tasty_undo_redo <&> \case
            Passed -> Left RunPass
            Defeat -> Left RunDefeat
            Fail tc sp failann -> Right (SkipToShrink tc sp, failann)
      -- This is essentially "recheckAt", with the skip/shrink info from above
      ExceptT $
        fmap Left $
          runProp seed (withSkip shrink tasty_undo_redo) >>= \case
            Passed -> do
              -- mapM_ (print . \(FailedAnnotation sp val) -> FailedAnnotation sp (take 40 val)) failann
              pure RecheckPass
            Defeat -> do
              -- mapM_ (print . \(FailedAnnotation sp val) -> FailedAnnotation sp (take 40 val)) failann
              pure RecheckDefeat
            Fail _ _ _ -> pure RecheckRefind

data RRInfo
  = RunPass
  | RunDefeat
  | RecheckPass
  | RecheckDefeat
  | RecheckRefind -- rechecking finds /an/ error, not asserted /the same/ error
  deriving stock (Show, Eq, Ord, Enum, Bounded)

showPad :: RRInfo -> Text
showPad ri = let s = show ri in s <> T.replicate (13 - T.length s) " "

data RunInfo
  = Passed
  | Defeat
  | Fail TestCount ShrinkPath [FailedAnnotation]

runProp :: Seed -> Property -> IO RunInfo
runProp seed prop = do
  report <- checkReport (propertyConfig prop) 0 seed (propertyTest prop) $ const $ pure ()
  let testcount = reportTests report
  let seed' = reportSeed report
  -- check my understanding
  unless (seed == seed') $ die "seed /= seed'"
  pure $ case reportStatus report of
    GaveUp -> Defeat
    OK -> Passed
    Failed x -> Fail testcount (failureShrinkPath x) (failureAnnotations x)

-- | A helper type for 'tasty_available_actions_actions',
-- describing where a particular option came from.
data Provenance
  = -- | This option was offered by the 'Available.options' API
    Offered
  | -- | This option is free-form entry. For example, this simulates
    -- renaming a definition to a hand-entered name.
    StudentProvided
  deriving stock (Show)

-- gives def name and perhaps a node inside it (if Nothing, then has selected the definition itself)
-- If the outer Maybe is Nothing, then there were no definitions at all!
pickPos :: Prog -> Maybe (Gen (GVarName, Either Def (ASTDef, NodeType, ID)))
pickPos p = ((\(defName, def) -> (defName,) <$> pickLoc def) =<<) <$> pickDef
  where
    pickDef = case Map.toList $ progAllDefs p of
      [] -> Nothing
      mut -> Just $ Gen.element mut
    pickLoc d =
      Gen.frequency $
        catMaybes
          [ Just (1, pure $ Left d)
          , defAST d <&> \d' -> (2,) . Gen.element $ fmap (Right . (d',SigNode,)) $ toListOf typeIDs $ astDefType d'
          , defAST d <&> \d' -> (7,) . Gen.element $ fmap (Right . (d',BodyNode,)) $ toListOf exprIDs $ astDefExpr d'
          ]

-- 'Nothing' means that a somewhat-expected problem occured:
-- - picked a node with no actions available
-- - picked an action with no options available
-- - entered a free-choice option and had name-clashing issues
runRandomAvailableAction :: Monad m => App -> PropertyT m (Maybe App)
runRandomAvailableAction a = do
  (defName, defLoc) <- maybe discard forAll (pickPos $ appProg a)
  let defMap = progAllDefs $ appProg a
  let (loc, acts) = case defLoc of
        Left _ -> (Nothing, Available.forDef defMap defName)
        Right (d, SigNode, i) -> (Just (SigNode, i), Available.forSig (astDefType d) i)
        Right (d, BodyNode, i) -> (Just (BodyNode, i), Available.forBody (astDefExpr d) i)
  case acts of
    [] -> label "no offered actions" >> pure Nothing
    acts' -> do
      action <- forAll $ Gen.element acts'
      collect action
      case action of
        Available.NoInput act' -> do
          progActs <-
            either (\e -> annotateShow e >> failure) pure $
              toProgActionNoInput defName loc act'
          Just <$> actionSucceeds (handleEditRequest progActs) a
        Available.Input act' -> do
          n <- forAll genName
          progActs <- either (const failure) pure $ toProgActionInput defName (Available.Option n) act'
          actionSucceedsOrCapture StudentProvided (handleEditRequest progActs) a
  where
    actionSucceeds :: (HasCallStack, Monad m) => EditAppM Identity ProgError a -> App -> PropertyT m App
    actionSucceeds m a' =
      runEditAppMLogs m a' & \case
        (Left _, _) -> failure
        (Right _, a'') -> pure a''
    -- If we submit our own name rather than an offered one, then
    -- we should expect that name capture/clashing may happen
    actionSucceedsOrCapture :: (HasCallStack, Monad m) => Provenance -> EditAppM Identity ProgError a -> App -> PropertyT m (Maybe App)
    actionSucceedsOrCapture p m a' = do
      case (p, runEditAppMLogs m a') of
        (StudentProvided, (Left (ActionError NameCapture), _)) -> do
          label "name-capture with entered name"
          pure Nothing
        (StudentProvided, (Left DefAlreadyExists{}, _)) -> do
          label "rename def name clash with entered name"
          pure Nothing
        (_, (Left _, _)) -> failure
        (_, (Right _, a''')) -> pure $ Just a'''

runEditAppMLogs :: EditAppM Identity ProgError a -> App -> (Either ProgError a, App)
runEditAppMLogs m a' = runIdentity $ runEditAppM m a'

prog :: MonadFresh ID m => m Prog
prog = do
  let a = "a6"
  e <- emptyHole `ann` tEmptyHole
  t <- tcon a
  let m =
        Module
          { moduleTypes =
              Map.fromList
                [
                  ( "a6"
                  , TypeDefAST ASTTypeDef{}
                  )
                ]
          , moduleDefs = Map.fromList [("a", DefAST $ ASTDef e t)]
          }
  pure $
    Prog
      { progModule = m
      , progSelection = Nothing
      }

tasty_undo_redo :: Property
tasty_undo_redo = withTests 500 $
  withDiscards 2000 $
    property $ do
      let (p',i) = create prog
      let a = mkApp i p'
      let n = 4
      void $ iterateNM n a $ \a' -> runRandomAction a'
  where
    runRandomAction a = fromMaybe a <$> runRandomAvailableAction a

iterateNM :: Monad m => Int -> a -> (a -> m a) -> m a
iterateNM n a f
  | n <= 0 = pure a
  | otherwise = f a >>= \fa -> iterateNM (n - 1) fa f

genName :: MonadGen m => m Text
genName = Gen.frequency [(9, fixed), (1, random)]
  where
    fixed = Gen.element ["x", "y", "z", "foo", "bar"]
    random = Gen.text (Range.linear 1 10) Gen.alpha
