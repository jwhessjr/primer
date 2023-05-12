{-# LANGUAGE OverloadedLabels #-}

module Main (main) where

import Foreword

import Control.Monad.Log (WithSeverity)
import Data.List.Extra (partition)
import Data.Map qualified as Map
import Data.Text qualified as T
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Optics (toListOf)
import Primer.Action (
  ActionError (CaseBindsClash, NameCapture),
  toProgActionInput,
  toProgActionNoInput, ProgAction (AddTypeDef,CreateDef),
 )
import Primer.Action.Available qualified as Available
import Primer.App (
  App,
  EditAppM,
  Editable (..),
  Level (..),
  NodeType (..),
  Prog (..),
  ProgError (ActionError, DefAlreadyExists),
  MutationRequest(Edit),
  appProg,
  handleEditRequest,
  progAllDefs,
  progAllTypeDefs,
  progCxt,
  progModules,
  runEditAppM, handleMutationRequest, MutationRequest (Undo), Log (..), tcWholeProgWithImports, mkApp,
 )
import Primer.Core (
  GVarName,
  ID,
  qualifyName, ModuleName (..),
 )
import Primer.Core.Utils (
  exprIDs,
  typeIDs,
 )
import Primer.Def (
  ASTDef (..),
  Def (DefAST),
  defAST,
 )
import Primer.Gen.App (extendCxtByModules)
import Primer.Gen.Core.Raw (genName)
import Primer.Gen.Core.Typed (WT, forAllT, propertyWT, freshNameForCxt, isolateWT)
import Primer.Log (PureLog, runPureLog)
import Primer.Module (
  Module (..),
 )
import Primer.Name (Name (unName))
import Primer.Test.Util (testNoSevereLogs)
import Primer.Typecheck (
  SmartHoles (SmartHoles), TypeError,
 )
import Primer.TypeDef (ASTTypeDef(..), TypeDef (..))
import Control.Monad.Fresh (fresh, MonadFresh)
import Primer.Core.DSL
import Hedgehog.Internal.Runner (checkReport)
import Hedgehog.Internal.Property (Property(propertyConfig, propertyTest), Skip(SkipToShrink), TestCount, ShrinkPath)
import qualified Hedgehog.Internal.Seed as Seed
import Hedgehog.Internal.Report (reportStatus, Report (reportSeed, reportTests), Result (..), FailureReport (failureShrinkPath))
import Numeric.Natural (Natural)
import qualified Data.Map.Strict as M
import Data.List.Extra (enumerate)

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
count as = M.unionsWith (+) $ M.fromList [(a,0) | a <- enumerate] : fmap (`M.singleton` 1) as

-- This runs the test once with a random seed, and
-- - if it fails then rechecks it with the reported skip/shrink, reporting whether it finds an error again
-- - if it passes or gives up, report that
runAndRecheck :: IO RRInfo
runAndRecheck = either identity absurd <$> runExceptT go
 where
   go :: ExceptT RRInfo IO Void
   go = do
    seed <- Seed.random
    shrink <- ExceptT $ runProp seed tasty_undo_redo <&> \case
      Passed -> Left RunPass
      Defeat -> Left RunDefeat
      Fail tc sp -> Right $ SkipToShrink tc sp
    -- This is essentially "recheckAt", with the skip/shrink info from above
    ExceptT $ fmap Left $ runProp seed (withSkip shrink tasty_undo_redo) <&> \case
      Passed -> RecheckPass
      Defeat -> RecheckDefeat
      Fail _ _ -> RecheckRefind

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
  | Fail TestCount ShrinkPath

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
        Failed x -> Fail testcount $ failureShrinkPath x

-- | A helper type for 'tasty_available_actions_actions',
-- describing where a particular option came from.
data Provenance
  = -- | This option was offered by the 'Available.options' API
    Offered
  | -- | This option is free-form entry. For example, this simulates
    -- renaming a definition to a hand-entered name.
    StudentProvided
  deriving stock (Show)

--gives def name and perhaps a node inside it (if Nothing, then has selected the definition itself)
-- If the outer Maybe is Nothing, then there were no definitions at all!
pickPos :: Prog -> Maybe (Gen (GVarName, Editable, Either Def (ASTDef, NodeType, ID)))
pickPos p = ((\(defName, (editable, def)) -> (defName, editable,) <$> pickLoc def) =<<) <$> pickDef
  where
    isMutable = \case
            Editable -> True
            NonEditable -> False
    pickDef = case partition (isMutable . fst . snd) $ Map.toList $ progAllDefs p of
        ([], []) -> Nothing
        (mut, []) -> Just $ Gen.element mut
        ([], immut) -> Just $ Gen.element immut
        (mut, immut) -> Just $ Gen.frequency [(9, Gen.element mut), (1, Gen.element immut)]
    pickLoc d =
          Gen.frequency $
            catMaybes
              [ Just (1, pure $ Left d)
              , defAST d <&> \d' -> (2,) . Gen.element $ fmap (Right . (d',SigNode,)) $ toListOf typeIDs $ astDefType d'
              , defAST d <&> \d' -> (7,) . Gen.element $ fmap (Right . (d',BodyNode,)) $ toListOf exprIDs $ astDefExpr d'
              ]

-- TODO: if I work in PropertyT, should I revive the labels I dropped?
-- 'Nothing' means that a somewhat-expected problem occured:
-- - picked a node with no actions available
-- - picked an action with no options available
-- - entered a free-choice option and had name-clashing issues
runRandomAvailableAction :: Level -> App -> PropertyT WT (Maybe App)
runRandomAvailableAction l a = do
      (defName,defMut,defLoc) <- maybe discard forAll (pickPos $ appProg a)
      let defMap = fmap snd $ progAllDefs $ appProg a
      let (def, loc,acts) = case defLoc of
            Left d -> (d, Nothing,Available.forDef defMap l defMut defName)
            Right (d,SigNode, i) -> (DefAST d, Just (SigNode, i), Available.forSig l defMut (astDefType d) i)
            Right (d,BodyNode, i) -> (DefAST d, Just (BodyNode, i), Available.forBody (snd <$> progAllTypeDefs (appProg a)) l defMut (astDefExpr d) i)
      case acts of
        [] -> label "no offered actions" >> pure Nothing
        acts' -> do
          action <- forAllT $ Gen.element acts'
          collect action
          case action of
            Available.NoInput act' -> do
              def' <- maybe (annotate "primitive def" >> failure) pure $ defAST def
              progActs <-
                either (\e -> annotateShow e >> failure) pure $
                  toProgActionNoInput (map snd $ progAllDefs $ appProg a) def' defName loc act'
              Just <$> actionSucceeds (handleEditRequest progActs) a
            Available.Input act' -> do
              def' <- maybe (annotate "primitive def" >> failure) pure $ defAST def
              Available.Options{Available.opts, Available.free} <-
                maybe (annotate "id not found" >> failure) pure $
                  Available.options
                    (map snd $ progAllTypeDefs $ appProg a)
                    (map snd $ progAllDefs $ appProg a)
                    (progCxt $ appProg a)
                    l
                    def'
                    loc
                    act'
              let opts' = [Gen.element $ (Offered,) <$> opts | not (null opts)]
              let opts'' =
                    opts' <> case free of
                      Available.FreeNone -> []
                      Available.FreeVarName -> [(StudentProvided,) . flip Available.Option Nothing <$> (unName <$> genName)]
                      Available.FreeInt -> [(StudentProvided,) . flip Available.Option Nothing <$> (show <$> Gen.integral (Range.linear @Integer 0 1_000_000_000))]
                      Available.FreeChar -> [(StudentProvided,) . flip Available.Option Nothing . T.singleton <$> Gen.unicode]
              case opts'' of
                [] -> annotate "no options" >> pure Nothing
                options -> do
                  opt <- forAllT $ Gen.choice options
                  progActs <- either (\e -> annotateShow e >> failure) pure $ toProgActionInput def' defName loc (snd opt) act'
                  actionSucceedsOrCapture (fst opt) (handleEditRequest progActs) a
  where
    runEditAppMLogs ::
      HasCallStack =>
      EditAppM (PureLog (WithSeverity ())) ProgError a ->
      App ->
      PropertyT WT (Either ProgError a, App)
    runEditAppMLogs m a' = case runPureLog $ runEditAppM m a' of
      (r, logs) -> testNoSevereLogs logs >> pure r
    actionSucceeds :: HasCallStack => EditAppM (PureLog (WithSeverity ())) ProgError a -> App -> PropertyT WT App
    actionSucceeds m a' =
      runEditAppMLogs m a' >>= \case
        (Left err, _) -> annotateShow err >> failure
        (Right _, a'') -> pure a''
    -- If we submit our own name rather than an offered one, then
    -- we should expect that name capture/clashing may happen
    actionSucceedsOrCapture :: HasCallStack => Provenance -> EditAppM (PureLog (WithSeverity ())) ProgError a -> App -> PropertyT WT (Maybe App)
    actionSucceedsOrCapture p m a' = do
      a'' <- runEditAppMLogs m a'
      case (p, a'') of
        (StudentProvided, (Left (ActionError NameCapture), _)) -> do
          label "name-capture with entered name"
          annotate "ignoring name capture error as was generated name, not offered one"
          pure Nothing
        (StudentProvided, (Left (ActionError (CaseBindsClash{})), _)) -> do
          label "name-clash with entered name"
          annotate "ignoring name clash error as was generated name, not offered one"
          pure Nothing
        (StudentProvided, (Left DefAlreadyExists{}, _)) -> do
          label "rename def name clash with entered name"
          annotate "ignoring def already exists error as was generated name, not offered one"
          pure Nothing
        (_, (Left err, _)) -> annotateShow err >> failure
        (_, (Right _, a''')) -> pure $ Just a'''

-- helper type for tasty_undo_redo
data Act = AddTm | AddTy
  | Un | Avail
  deriving stock Show

prog :: MonadFresh ID m => m Prog
prog = do
  let modName =  ModuleName { unModuleName = "M" :| [ "0" ] }
      a = qualifyName modName "a6"
  e <- lam "x" (case_ emptyHole [] `ann` tEmptyHole) `ann` (tcon a `tfun` tcon a)
  t <- tcon a `tfun` tcon a
  let m = Module
                { moduleName = modName
                , moduleTypes =
                    Map.fromList
                      [ ( "a6"
                        , TypeDefAST
                            ASTTypeDef
                              { astTypeDefParameters = []
                              , astTypeDefConstructors = []
                              , astTypeDefNameHints = []
                              }
                        )]
                , moduleDefs = Map.fromList [ ( "a" , DefAST $ ASTDef e t)]
                }
  pure $ Prog
        { progImports = []
        , progModules = [m]
        , progSelection = Nothing
        , progSmartHoles = SmartHoles
        , progLog = Log { unlog = [] }
        }

tasty_undo_redo :: Property
tasty_undo_redo = withTests 500 $
  withDiscards 2000 $
    propertyWT [] $ do
      let l = Expert
      -- We only test SmartHoles mode (which is the only supported user-facing
      -- mode - NoSmartHoles is only used for internal sanity testing etc)
      let annotateShow' :: HasCallStack => App -> PropertyT WT ()
          annotateShow' = withFrozenCallStack $ annotateShow . (\p -> (progModules p, progLog p)) . appProg
      Right p' <- runExceptT @TypeError $ tcWholeProgWithImports =<< prog
      i <- lift $ isolateWT fresh
      nc <- lift $ isolateWT fresh
      let a = mkApp i nc p'
      annotateShow' a
      n <- forAll $ Gen.int $ Range.linear 1 20
      a' <- iterateNM n a $ \a' -> runRandomAction l a'
      annotateShow' a'
      if null $ unlog $ progLog $ appProg a' -- TODO: expose a "log-is-null" helper from App?
        -- It is possible for the random actions to undo everything!
        then success
        else do
          a'' <- runEditAppMLogs (handleMutationRequest Undo) a'
          annotateShow' a''
  where
    -- TODO: dry
    runEditAppMLogs ::
      HasCallStack =>
      EditAppM (PureLog (WithSeverity ())) ProgError a ->
      App ->
      PropertyT WT App
    runEditAppMLogs m a = case runPureLog $ runEditAppM m a of
      (r, logs) -> testNoSevereLogs logs >> case r of
        (Left err, _) -> annotateShow err >> failure
        (Right _, a') -> pure a'
    runRandomAction l a = do
      act <- forAll $ Gen.frequency $ second pure <$> [
        (2,AddTm)
        ,(1,AddTy)
        ,(if null $ unlog $ progLog $ appProg a then 0 else 1,Un) -- TODO: expose a "log-is-null" helper from App?
        ,(5,Avail)
                                    ]
      case act of
        AddTm -> do
          let n' = local (extendCxtByModules $ progModules $ appProg a) freshNameForCxt
          n <- forAllT $ Gen.choice [Just . unName <$> n', pure Nothing]
          m <- forAllT $ Gen.element $ fmap moduleName $ progModules $ appProg a
          runEditAppMLogs (handleMutationRequest $ Edit [CreateDef m n]) a
        AddTy -> do
          m <- forAllT $ Gen.element $ fmap moduleName $ progModules $ appProg a
          let n' = local (extendCxtByModules $ progModules $ appProg a) freshNameForCxt
          n <- qualifyName m <$> forAllT n'
          runEditAppMLogs (handleMutationRequest $ Edit [AddTypeDef n $ ASTTypeDef [] [] []]) a
        Un -> runEditAppMLogs (handleMutationRequest Undo) a
        Avail -> fromMaybe a <$> runRandomAvailableAction l a

iterateNM :: Monad m => Int -> a -> (a -> m a) -> m a
iterateNM n a f
  | n <= 0 = pure a
  | otherwise = f a >>= \fa -> iterateNM (n - 1) fa f
