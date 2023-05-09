{-# LANGUAGE OverloadedLabels #-}

module Tests.Action.Available where

import Foreword

import Control.Monad.Log (WithSeverity)
import Data.ByteString.Lazy.Char8 qualified as BS
import Data.List.Extra (enumerate, partition)
import Data.Map qualified as Map
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import GHC.Err (error)
import Hedgehog (
  PropertyT,
  annotate,
  annotateShow,
  collect,
  discard,
  failure,
  label,
  success,
  (===), Gen, forAll,
 )
import Hedgehog.Gen qualified as Gen
import Hedgehog.Internal.Property (forAllWithT)
import Hedgehog.Range qualified as Range
import Optics (ix, toListOf, (%), (.~), (^..), _head)
import Primer.Action (
  ActionError (CaseBindsClash, NameCapture),
  Movement (Child1, Child2),
  moveExpr,
  toProgActionInput,
  toProgActionNoInput, ProgAction (AddTypeDef,CreateDef),
 )
import Primer.Action.Available (NoInputAction (Raise))
import Primer.Action.Available qualified as Available
import Primer.App (
  App,
  EditAppM,
  Editable (..),
  Level (Beginner),
  NodeType (..),
  Prog (progLog, redoLog),
  ProgError (ActionError, DefAlreadyExists),
  MutationRequest(Edit),
  appProg,
  checkAppWellFormed,
  handleEditRequest,
  progAllDefs,
  progAllTypeDefs,
  progCxt,
  progImports,
  progModules,
  progSmartHoles,
  runEditAppM, handleMutationRequest, MutationRequest (Undo, Redo), Log (unlog),
 )
import Primer.Core (
  Expr,
  GVarName,
  GlobalName (baseName, qualifiedModule),
  HasID (_id),
  ID,
  ModuleName (ModuleName),
  getID,
  mkSimpleModuleName,
  moduleNamePretty,
  qualifyName,
  _typeMeta,
 )
import Primer.Core.DSL (
  S,
  ann,
  app,
  create,
  create',
  emptyHole,
  gvar,
  lam,
  lvar,
  tEmptyHole,
 )
import Primer.Core.Utils (
  exprIDs,
  typeIDs,
 )
import Primer.Def (
  ASTDef (..),
  Def (DefAST, DefPrim),
  defAST,
 )
import Primer.Examples (comprehensiveWellTyped)
import Primer.Gen.App (genApp)
import Primer.Gen.Core.Raw (genName)
import Primer.Gen.Core.Typed (WT, forAllT, propertyWT, freshNameForCxt)
import Primer.Gen.App (extendCxtByModules)
import Primer.Log (PureLog, runPureLog)
import Primer.Module (
  Module (Module, moduleDefs, moduleName),
  builtinModule,
  moduleDefsQualified,
  moduleTypesQualified,
  primitiveModule,
 )
import Primer.Name (Name (unName))
import Primer.Test.TestM (evalTestM)
import Primer.Test.Util (clearMeta, testNoSevereLogs)
import Primer.Typecheck (
  CheckEverythingRequest (CheckEverything, toCheck, trusted),
  SmartHoles (NoSmartHoles, SmartHoles),
  buildTypingContextFromModules,
  checkEverything,
 )
import Primer.Zipper (focus)
import System.FilePath ((</>))
import Tasty (Property, withDiscards, withTests)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, (@?=))
import Tests.Action.Prog (defaultEmptyProg, expectSuccess, findGlobalByName, gvn, progActionTest)
import Tests.Typecheck (TypeCacheAlpha (TypeCacheAlpha), runTypecheckTestMIn)
import Text.Pretty.Simple (pShowNoColor)
import Primer.TypeDef (ASTTypeDef(ASTTypeDef))

-- | Comprehensive DSL test.
test_1 :: TestTree
test_1 = mkTests [create' builtinModule] $ create' $ comprehensiveWellTyped $ mkSimpleModuleName "M"

data Output = Output
  { defActions :: [OfferedAction]
  , bodyActions :: [(ID, [OfferedAction])]
  , sigActions :: [(ID, [OfferedAction])]
  }
  deriving stock (Show)
data OfferedAction
  = NoInput Available.NoInputAction
  | Input Available.InputAction Available.Options
  deriving stock (Show)

-- | Golden tests for the available actions at each node of the definition, for each level.
mkTests :: [Module] -> (GVarName, Def) -> TestTree
mkTests _ (_, DefPrim _) = error "mkTests is unimplemented for primitive definitions."
mkTests deps (defName, DefAST def') =
  let d = defName
      m = Module (qualifiedModule d) mempty $ Map.singleton (baseName d) $ DefAST def'
      cxt = buildTypingContextFromModules deps NoSmartHoles
      def = case runTypecheckTestMIn
        cxt
        (checkEverything NoSmartHoles CheckEverything{trusted = deps, toCheck = [m]}) of
        Left err -> error $ "mkTests: no typecheck: " <> show err
        Right [m'] -> case Map.toList $ moduleDefs m' of
          [(_, DefAST def'')] -> def''
          _ -> error "mkTests: expected exactly one definition in checked module"
        _ -> error "mkTests: expected exactly one module checked modules"
      testName = T.unpack $ moduleNamePretty (qualifiedModule defName) <> "." <> unName (baseName defName)
      enumeratePairs = (,) <$> enumerate <*> enumerate
      defs = Map.singleton defName $ DefAST def
      typeDefs = foldMap' @[] moduleTypesQualified [create' builtinModule, primitiveModule]
      offered level id = \case
        Available.NoInput a -> NoInput a
        Available.Input a ->
          Input a
            . fromMaybe (error "id not found")
            $ Available.options typeDefs defs cxt level def id a
   in testGroup testName $
        enumeratePairs
          <&> \(level, mut) ->
            let defActions = map (offered level Nothing) $ Available.forDef defs level mut d
                bodyActions =
                  map
                    ( \id ->
                        ( id
                        , map (offered level (Just (BodyNode, id))) $
                            Available.forBody
                              typeDefs
                              level
                              mut
                              (astDefExpr def)
                              id
                        )
                    )
                    . toListOf exprIDs
                    $ astDefExpr def
                sigActions =
                  map
                    ( \id ->
                        ( id
                        , map (offered level (Just (SigNode, id))) $ Available.forSig level mut (astDefType def) id
                        )
                    )
                    . toListOf (_typeMeta % _id)
                    $ astDefType def
             in goldenVsString (show level <> ":" <> show mut) ("test/outputs/available-actions" </> testName </> show level <> "-" <> show mut <> ".fragment") $
                  pure . BS.fromStrict . encodeUtf8 . TL.toStrict . pShowNoColor $
                    Output
                      { defActions
                      , bodyActions
                      , sigActions
                      }

-- We should not offer to delete a definition that is in use, as that
-- action cannot possibly succeed
unit_def_in_use :: Assertion
unit_def_in_use =
  let (d, defs) = create' $ do
        let foo = qualifyName (ModuleName ["M"]) "foo"
        fooDef <- ASTDef <$> emptyHole <*> tEmptyHole
        let bar = qualifyName (ModuleName ["M"]) "bar"
        barDef <- ASTDef <$> gvar foo <*> tEmptyHole
        let ds = [(foo, DefAST fooDef), (bar, DefAST barDef)]
        pure (foo, Map.fromList ds)
   in for_
        enumerate
        ( \l ->
            Available.forDef defs l Editable d
              @?= [Available.Input Available.RenameDef, Available.NoInput Available.DuplicateDef]
        )

-- | A helper type for 'tasty_available_actions_actions',
-- describing where a particular option came from.
data Provenance
  = -- | This option was offered by the 'Available.options' API
    Offered
  | -- | This option is free-form entry. For example, this simulates
    -- renaming a definition to a hand-entered name.
    StudentProvided
  deriving stock (Show)

tasty_available_actions_accepted :: Property
tasty_available_actions_accepted = withTests 500 $
  withDiscards 2000 $
    propertyWT [] $ do
      l <- forAllT $ Gen.element enumerate
      cxt <- forAllT $ Gen.choice $ map sequence [[], [builtinModule], [builtinModule, pure primitiveModule]]
      -- We only test SmartHoles mode (which is the only supported user-facing
      -- mode - NoSmartHoles is only used for internal sanity testing etc)
      a <- forAllT $ genApp SmartHoles cxt
      (defName,defMut,defLoc) <- maybe discard forAll (pickPos $ appProg a)
      -- TODO: use runRandomAction
      let defMap = fmap snd $ progAllDefs $ appProg a
      let (def, loc,acts) = case defLoc of
            Left d -> (d, Nothing,Available.forDef defMap l defMut defName)
            Right (d,SigNode, i) -> (DefAST d, Just (SigNode, i), Available.forSig l defMut (astDefType d) i)
            Right (d,BodyNode, i) -> (DefAST d, Just (BodyNode, i), Available.forBody (snd <$> progAllTypeDefs (appProg a)) l defMut (astDefExpr d) i)
      case acts of
        [] -> label "no offered actions" >> success
        acts' -> do
          action <- forAllT $ Gen.element acts'
          collect action
          case action of
            Available.NoInput act' -> do
              def' <- maybe (annotate "primitive def" >> failure) pure $ defAST def
              progActs <-
                either (\e -> annotateShow e >> failure) pure $
                  toProgActionNoInput (map snd $ progAllDefs $ appProg a) def' defName loc act'
              actionSucceeds (handleEditRequest progActs) a
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
                [] -> annotate "no options" >> success
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
    runEditAppMLogs m a = case runPureLog $ runEditAppM m a of
      (r, logs) -> testNoSevereLogs logs >> pure r
    actionSucceeds :: HasCallStack => EditAppM (PureLog (WithSeverity ())) ProgError a -> App -> PropertyT WT ()
    actionSucceeds m a =
      runEditAppMLogs m a >>= \case
        (Left err, _) -> annotateShow err >> failure
        (Right _, a') -> ensureSHNormal a'
    -- If we submit our own name rather than an offered one, then
    -- we should expect that name capture/clashing may happen
    actionSucceedsOrCapture :: HasCallStack => Provenance -> EditAppM (PureLog (WithSeverity ())) ProgError a -> App -> PropertyT WT ()
    actionSucceedsOrCapture p m a = do
      a' <- runEditAppMLogs m a
      case (p, a') of
        (StudentProvided, (Left (ActionError NameCapture), _)) -> do
          label "name-capture with entered name"
          annotate "ignoring name capture error as was generated name, not offered one"
        (StudentProvided, (Left (ActionError (CaseBindsClash{})), _)) -> do
          label "name-clash with entered name"
          annotate "ignoring name clash error as was generated name, not offered one"
        (StudentProvided, (Left DefAlreadyExists{}, _)) -> do
          label "rename def name clash with entered name"
          annotate "ignoring def already exists error as was generated name, not offered one"
        (_, (Left err, _)) -> annotateShow err >> failure
        (_, (Right _, a'')) -> ensureSHNormal a''
    ensureSHNormal a = case checkAppWellFormed a of
      Left err -> annotateShow err >> failure
      Right a' -> TypeCacheAlpha a === TypeCacheAlpha a' >> checkUndo a
    checkUndo a = runEditAppMLogs (handleMutationRequest Undo) a >>= \case
        (Left err, _) -> annotateShow err >> failure
        (Right _, a') -> runEditAppMLogs (handleMutationRequest Redo) a' >>= \case
          (Left err, _) -> annotateShow err >> failure
          (Right _, a'') -> TypeCacheAlpha a === TypeCacheAlpha a''

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
    runEditAppMLogs m a = case runPureLog $ runEditAppM m a of
      (r, logs) -> testNoSevereLogs logs >> pure r
    actionSucceeds :: HasCallStack => EditAppM (PureLog (WithSeverity ())) ProgError a -> App -> PropertyT WT App
    actionSucceeds m a =
      runEditAppMLogs m a >>= \case
        (Left err, _) -> annotateShow err >> failure
        (Right _, a') -> pure a'
    -- If we submit our own name rather than an offered one, then
    -- we should expect that name capture/clashing may happen
    actionSucceedsOrCapture :: HasCallStack => Provenance -> EditAppM (PureLog (WithSeverity ())) ProgError a -> App -> PropertyT WT (Maybe App)
    actionSucceedsOrCapture p m a = do
      a' <- runEditAppMLogs m a
      case (p, a') of
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
        (_, (Right _, a'')) -> pure $ Just a''

-- helper type for tasty_undo_redo
data Act = AddTm | AddTy
  | Un | Re | Avail
  deriving stock Show

tasty_undo_redo :: Property
tasty_undo_redo = withTests 500 $
  withDiscards 2000 $
    propertyWT [] $ do
      l <- forAllT $ Gen.element enumerate
      cxt <- forAllT $ Gen.choice $ map sequence [[], [builtinModule], [builtinModule, pure primitiveModule]]
      -- We only test SmartHoles mode (which is the only supported user-facing
      -- mode - NoSmartHoles is only used for internal sanity testing etc)
      let annotateShow' :: HasCallStack => App -> PropertyT WT ()
          annotateShow' = withFrozenCallStack $ annotateShow . (\p -> (progModules p, progLog p, redoLog p)) . appProg
      a <- forAllT $ genApp SmartHoles cxt
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
          a''' <- runEditAppMLogs (handleMutationRequest Redo) a''
          annotateShow' a'''
          TypeCacheAlpha a' === TypeCacheAlpha a'''
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
        ,(if null $ unlog $ redoLog $ appProg a then 0 else 1,Re) -- TODO: expose a "log-is-null" helper from App?
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
        Re -> runEditAppMLogs (handleMutationRequest Redo) a
        Avail -> fromMaybe a <$> runRandomAvailableAction l a


iterateNM :: Monad m => Int -> a -> (a -> m a) -> m a
iterateNM n a f
  | n <= 0 = pure a
  | otherwise = f a >>= \fa -> iterateNM (n - 1) fa f

-- 'Raise' works when moving checkable terms into synthesisable position
unit_raise_sh :: Assertion
unit_raise_sh =
  let test :: HasCallStack => S Expr -> S Expr -> Assertion
      test t1 t2 =
        offeredActionTest
          SmartHoles
          Beginner
          (emptyHole `app` t1 `app` emptyHole)
          [Child1, Child2]
          Raise
          (t2 `app` emptyHole)
      testSyn :: HasCallStack => S Expr -> Assertion
      testSyn e = test e e
      testChk :: HasCallStack => S Expr -> Assertion
      testChk t = test t (t `ann` tEmptyHole)
   in do
        testSyn emptyHole
        testChk $ lam "x" (lvar "x")

-- | Apply the action to the node in the input expression pointed to by the
-- 'Movement' (starting from the root), checking that it would actually be offered
-- there, and then checking the result matches the expected output, up to renaming
-- of IDs and changing cached types.
offeredActionTest ::
  HasCallStack =>
  SmartHoles ->
  Level ->
  S Expr ->
  [Movement] ->
  NoInputAction ->
  S Expr ->
  Assertion
offeredActionTest sh l inputExpr position action expectedOutput = do
  let ((modules, expr, exprDef, exprDefName, prog), i) = create $ do
        ms <- sequence [builtinModule]
        prog0 <- defaultEmptyProg
        e <- inputExpr
        d <- ASTDef e <$> tEmptyHole
        let p =
              prog0
                & (#progModules % _head % #moduleDefs % ix "main" .~ DefAST d)
                & (#progImports .~ ms)
                & (#progSmartHoles .~ sh)
        pure (ms, e, d, gvn "main", p)
  let id' = evalTestM (i + 1) $
        runExceptT $
          flip runReaderT (buildTypingContextFromModules modules sh) $
            do
              ez <- foldlM (flip moveExpr) (focus expr) position
              pure $ getID ez
  id <- case id' of
    Left err -> assertFailure $ show err
    Right i' -> pure i'
  let offered = Available.forBody (foldMap' moduleTypesQualified modules) l Editable expr id
  assertBool "Requested action was not offered" (Available.NoInput action `elem` offered)
  action' <- case toProgActionNoInput (foldMap' moduleDefsQualified $ progModules prog) exprDef exprDefName (Just (BodyNode, id)) action of
    Left err -> assertFailure $ show err
    Right a -> pure a
  let expected = create' expectedOutput
  progActionTest (pure prog) action' $ expectSuccess $ \_ prog' ->
    let result = pure . astDefExpr <=< defAST <=< findGlobalByName prog' $ exprDefName
     in -- Compare result to input, ignoring any difference in metadata
        -- NB: we don't compare up-to-alpha, as names should be determined by the
        -- actions on-the-nose
        fmap clearMeta result @?= Just (clearMeta expected)
