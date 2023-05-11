{-# LANGUAGE OverloadedLabels #-}

import Foreword

import Control.Monad.Log (WithSeverity)
import Data.List.Extra (enumerate, partition)
import Data.Map qualified as Map
import Data.Text qualified as T
import Hedgehog (
  check,
  PropertyT,
  Property, withDiscards, withTests,
  annotate,
  annotateShow,
  collect,
  discard,
  failure,
  label,
  success,
  (===), Gen, forAll, Seed (Seed), recheckAt,
 )
import Hedgehog.Gen qualified as Gen
import Hedgehog.Main (defaultMain)
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
  Level,
  NodeType (..),
  Prog (progLog, redoLog),
  ProgError (ActionError, DefAlreadyExists),
  MutationRequest(Edit),
  appProg,
  handleEditRequest,
  progAllDefs,
  progAllTypeDefs,
  progCxt,
  progModules,
  runEditAppM, handleMutationRequest, MutationRequest (Undo, Redo), Log (unlog),
 )
import Primer.Core (
  GVarName,
  ID,
  qualifyName,
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
import Primer.Gen.App (extendCxtByModules, genApp)
import Primer.Gen.Core.Raw (genName)
import Primer.Gen.Core.Typed (WT, forAllT, propertyWT, freshNameForCxt)
import Primer.Log (PureLog, runPureLog)
import Primer.Module (
  Module (moduleName),
  builtinModule,
  primitiveModule,
 )
import Primer.Name (Name (unName))
import Primer.Test.Util (testNoSevereLogs)
import Primer.Typecheck (
  SmartHoles (SmartHoles),
 )
import Tests.Typecheck (TypeCacheAlpha (TypeCacheAlpha))
import Primer.TypeDef (ASTTypeDef(ASTTypeDef))

main :: IO ()
--main = defaultMain [ check tasty_undo_redo ]
main = recheckAt (Seed 11179484540861538616 16916482896198407115) "241:aDf" tasty_undo_redo

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
