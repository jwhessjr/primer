{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ViewPatterns #-}

-- This module defines the high level application functions.

module Primer.App (
  module Primer.App.Base,
  Log (..),
  defaultLog,
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
  progAllTypeDefs,
  allValConNames,
  allTyConNames,
  progCxt,
  tcWholeProg,
  tcWholeProgWithImports,
  ProgAction (..),
  ProgError (..),
  Question (..),
  handleMutationRequest,
  handleEditRequest,
  MutationRequest (..),
  Selection (..),
  NodeSelection (..),
  EvalReq (..),
  EvalResp (..),
  EvalFullReq (..),
  EvalFullResp (..),
  lookupASTDef,
  liftError,
) where

import Foreword hiding (mod)

import Control.Monad.Fresh (MonadFresh (..))
import Control.Monad.Log (MonadLog, WithSeverity)
import Control.Monad.NestedError (MonadNestedError)
import Data.Data (Data)
import Data.Generics.Uniplate.Operations (descendM, transform, transformM)
import Data.Generics.Uniplate.Zipper (
  fromZipper,
 )
import Data.List.Extra ((!?))
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Optics (
  Field1 (_1),
  Field2 (_2),
  Field3 (_3),
  ReversibleOptic (re),
  Setter',
  ifoldMap,
  lens,
  mapped,
  over,
  set,
  sets,
  traverseOf,
  traversed,
  view,
  (%),
  (%~),
  (.~),
  (?~),
  (^.),
  (^?),
  _Just,
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
import Primer.App.Base (
  Editable (..),
  Level (..),
  NodeType (..),
 )
import Primer.Core (
  Bind' (Bind),
  CaseBranch,
  CaseBranch' (CaseBranch),
  Expr,
  Expr' (Case, Con, EmptyHole, Hole, Var),
  ExprMeta,
  GVarName,
  GlobalName (baseName, qualifiedModule),
  HasID (_id),
  ID (..),
  LocalName (LocalName, unLocalName),
  Meta (..),
  ModuleName (ModuleName),
  TmVarRef (GlobalVarRef, LocalVarRef),
  TyConName,
  Type,
  Type' (..),
  TypeCache (TCSynthed),
  TypeMeta,
  ValConName,
  getID,
  qualifyName,
  typesInExpr,
  unModuleName,
  unsafeMkGlobalName,
  unsafeMkLocalName,
  _chkedAt,
  _exprMetaLens,
  _synthed,
  _typeMetaLens,
 )
import Primer.Core.DSL (ann, hole, tEmptyHole, tvar)
import Primer.Core.DSL qualified as DSL
import Primer.Core.Transform (foldApp, renameVar, unfoldAPP, unfoldApp, unfoldTApp)
import Primer.Core.Utils (freeVars, generateTypeIDs, regenerateExprIDs, regenerateTypeIDs, _freeTmVars, _freeTyVars, _freeVarsTy)
import Primer.Def (
  ASTDef (..),
  _astDefExpr,
  _astDefType,
  Def (..),
  _DefAST,
  DefMap,
  defAST,
 )
import Primer.Def.Utils (globalInUse)
import Primer.Eval.Detail (EvalDetail)
import Primer.EvalFull (Dir, TerminationBound)
import Primer.Module (
  Module (moduleDefs, moduleName, moduleTypes),
  _moduleTypes,
  _moduleDefs,
  deleteDef,
  insertDef,
  moduleDefsQualified,
  moduleTypesQualified,
  qualifyDefName,
  renameModule,
  renameModule',
 )
import Primer.Name (Name (unName), NameCounter, freshName, unsafeMkName)
import Primer.Questions (
  Question (..),
 )
import Primer.TypeDef (
  ASTTypeDef (..),
  _astTypeDefConstructors,
  _astTypeDefParameters,
  TypeDef (..),
  _TypeDefAST,
  TypeDefMap,
  ValCon (..),
  _valConArgs,
  _valConName,
  generateTypeDefIDs,
  typeDefAST,
 )
import Primer.Typecheck (
  CheckEverythingRequest (CheckEverything, toCheck, trusted),
  Cxt,
  SmartHoles (NoSmartHoles),
  TypeError,
  buildTypingContextFromModules,
  checkEverything,
  checkTypeDefs,
  synth,
 )
import Primer.Typecheck.Utils (_typecache)
import Primer.Zipper (
  ExprZ,
  Loc' (InBind, InExpr, InType),
  TypeZ,
  TypeZip,
  current,
  focusOn,
  focusOnTy,
  focusOnlyType,
  foldAbove,
  foldAboveTypeZ,
  getBoundHere,
  getBoundHereUp,
  getBoundHereUpTy,
  locToEither,
  replace,
  target,
  unfocusExpr,
  unfocusType,
  _target,
 )

-- | The full program state.
data Prog = Prog
  { progImports :: [Module]
  -- ^ Some immutable imported modules
  , progModules :: [Module]
  -- ^ The editable "home" modules
  , progSelection :: Maybe Selection
  , progSmartHoles :: SmartHoles
  , progLog :: Log
  -- ^ The sequence of all successful edits performed on the program
  -- since its creation, in order of first edit to last. A successful
  -- undo operation pops the last edit from this log, and pushes it
  -- onto the 'redoLog'.
  , redoLog :: Log
  -- ^ A log of successive undo operations, in order of most recent to
  -- least (i.e., a LIFO-order stack). A successful redo operation
  -- pops the most recent undo from this log. Note that this log is
  -- reset whenever an action is performed; i.e., redos are not
  -- preserved across edits.
  }
  deriving stock (Eq, Show, Read)

_progImports :: Setter' Prog [Module]
_progImports = sets $ \f p -> p {progImports = f $ progImports p}

_progModules :: Setter' Prog [Module]
_progModules = sets $ \f p -> p {progModules = f $ progModules p}

_progSelection :: Setter' Prog (Maybe Selection)
_progSelection = sets $ \f p -> p {progSelection = f $ progSelection p}

_progSmartHoles :: Setter' Prog SmartHoles
_progSmartHoles = sets $ \f p -> p {progSmartHoles = f $ progSmartHoles p}

_redoLog :: Setter' Prog Log
_redoLog = sets $ \f p -> p {redoLog = f $ redoLog p}

-- | Push a compound action onto the given 'Log', returning the new
-- 'Log'.
push :: [ProgAction] -> Log -> Log
push as l = Log $ as : unlog l

-- | Pop the head of the given 'Log'.
--
-- If the log is not empty, returns 'Just (as, l)' where 'as' is the
-- head of the log and 'l' is the rest of the log.
--
-- If the log is empty, returns 'Nothing'.
pop :: Log -> Maybe ([ProgAction], Log)
pop l = case unlog l of
  [] -> Nothing
  (as : l') -> Just (as, Log l')

progAllModules :: Prog -> [Module]
progAllModules p = progModules p <> progImports p

progAllTypeDefs :: Prog -> Map TyConName (Editable, TypeDef ())
progAllTypeDefs p =
  foldMap' (fmap (Editable,) . moduleTypesQualified) (progModules p)
    <> foldMap' (fmap (NonEditable,) . moduleTypesQualified) (progImports p)

progAllDefs :: Prog -> Map GVarName (Editable, Def)
progAllDefs p =
  foldMap' (fmap (Editable,) . moduleDefsQualified) (progModules p)
    <> foldMap' (fmap (NonEditable,) . moduleDefsQualified) (progImports p)

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

-- | Get all type definitions from all modules (including imports)
allTypes :: Prog -> TypeDefMap
allTypes = fmap snd . progAllTypeDefs

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

-- | The default (empty) 'Log'.
defaultLog :: Log
defaultLog = Log mempty

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
  | Redo
  | Edit [ProgAction]
  deriving stock (Eq, Show, Read)

data EvalReq = EvalReq
  { evalReqExpr :: Expr
  , evalReqRedex :: ID
  }
  deriving stock (Eq, Show, Read)

data EvalResp = EvalResp
  { evalRespExpr :: Expr
  , evalRespRedexes :: [ID]
  , evalRespDetail :: EvalDetail
  }
  deriving stock (Eq, Show, Read)

data EvalFullReq = EvalFullReq
  { evalFullReqExpr :: Expr
  , evalFullCxtDir :: Dir -- is this expression in a syn/chk context, so we can tell if is an embedding.
  , evalFullMaxSteps :: TerminationBound
  }
  deriving stock (Eq, Show, Read)

-- If we time out, we still return however far we got
data EvalFullResp
  = EvalFullRespTimedOut Expr
  | EvalFullRespNormal Expr
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
  Redo -> handleRedoRequest

-- | Handle an edit request
--
-- Note that a successful edit resets the redo log.
handleEditRequest :: forall m l. MonadEditApp l ProgError m => [ProgAction] -> m Prog
handleEditRequest actions = do
  (prog, _) <- gets appProg >>= \p -> foldlM go (p, Nothing) actions
  let l = progLog prog
  let prog' = prog{progLog = push actions l, redoLog = defaultLog}
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
  CreateDef modName n -> editModule modName prog $ \mod -> do
    let defs = moduleDefs mod
    name <- case n of
      Just nameStr ->
        let baseName = unsafeMkName nameStr
            name = qualifyName modName baseName
         in if Map.member baseName defs
              then throwError $ DefAlreadyExists name
              else pure baseName
      Nothing -> freshName $ Map.keysSet defs
    expr <- newExpr
    ty <- newType
    let def = ASTDef expr ty
    pure (insertDef mod name $ DefAST def, Just $ Selection (qualifyName modName name) Nothing)
  AddTypeDef tc td -> editModuleSameSelection (qualifiedModule tc) prog $ \m -> do
    td' <- generateTypeDefIDs $ TypeDefAST td
    let tydefs' = moduleTypes m <> Map.singleton (baseName tc) td'
    m{moduleTypes = tydefs'}
      <$ liftError
        -- The frontend should never let this error case happen,
        -- so we just dump out a raw string for debugging/logging purposes
        -- (This is not currently true! We should synchronise the frontend with
        -- the typechecker rules. For instance, the form allows to create
        --   data T (T : *) = T
        -- but the TC rejects it.
        -- see https://github.com/hackworthltd/primer/issues/3)
        (TypeDefError . show @TypeError)
        ( runReaderT
            (checkTypeDefs $ Map.singleton tc (TypeDefAST td))
            (buildTypingContextFromModules (progAllModules prog) NoSmartHoles)
        )
  RenameType old (unsafeMkName -> nameRaw) -> editModuleSameSelectionCross (qualifiedModule old) prog $ \(m, ms) -> do
    when (new `elem` allTyConNames prog) $ throwError $ TypeDefAlreadyExists new
    m' <- traverseOf _moduleTypes updateType m
    let renamedInTypes = over (traversed % _moduleTypes) updateRefsInTypes $ m' : ms
    pure $ over (traversed % _moduleDefs % traversed % _DefAST) (updateDefBody . updateDefType) renamedInTypes
    where
      new = qualifyName (qualifiedModule old) nameRaw
      updateType m = do
        d0 <-
          -- NB We do not allow primitive types to be renamed.
          -- To relax this, we'd have to be careful about how it interacts with type-checking of primitive literals.
          maybe (throwError $ TypeDefIsPrim old) pure . typeDefAST
            =<< maybe (throwError $ TypeDefNotFound old) pure (Map.lookup (baseName old) m)
        when (nameRaw `elem` map (unLocalName . fst) (astTypeDefParameters d0)) $ throwError $ TyConParamClash nameRaw
        pure $ Map.insert nameRaw (TypeDefAST d0) $ Map.delete (baseName old) m
      updateRefsInTypes =
        over
          (traversed % _TypeDefAST % _astTypeDefConstructors % traversed % _valConArgs % traversed)
          $ transform
          $ over (#_TCon % _2) updateName
      updateDefType =
        over
          _astDefType
          $ transform
          $ over (#_TCon % _2) updateName
      updateDefBody =
        over
          _astDefExpr
          $ transform
          $ over typesInExpr
          $ transform
          $ over (#_TCon % _2) updateName
      updateName n = if n == old then new else n
  RenameCon type_ old (unsafeMkGlobalName . (fmap unName (unModuleName (qualifiedModule type_)),) -> new) ->
    editModuleSameSelectionCross (qualifiedModule type_) prog $ \(m, ms) -> do
      when (new `elem` allValConNames prog) $ throwError $ ConAlreadyExists new
      m' <- updateType m
      pure $ over (mapped % _moduleDefs) updateDefs (m' : ms)
    where
      updateType =
        alterTypeDef
          ( traverseOf
              _astTypeDefConstructors
              ( maybe (throwError $ ConNotFound old) pure
                  . findAndAdjust ((== old) . valConName) (_valConName .~ new)
              )
          )
          type_
      updateDefs =
        over (traversed % _DefAST % _astDefExpr) $
          transform $
            over (#_Con % _2) updateName
              . over (#_Case % _3 % traversed % #_CaseBranch % _1) updateName
      updateName n = if n == old then new else n
  RenameTypeParam type_ old (unsafeMkLocalName -> new) ->
    editModuleSameSelection (qualifiedModule type_) prog updateType
    where
      updateType =
        alterTypeDef
          (updateConstructors <=< updateParam)
          type_
      updateParam def = do
        when (new `elem` map fst (astTypeDefParameters def)) $ throwError $ ParamAlreadyExists new
        let nameRaw = unLocalName new
        when (nameRaw == baseName type_) $ throwError $ TyConParamClash nameRaw
        when (nameRaw `elem` map (baseName . valConName) (astTypeDefConstructors def)) $ throwError $ ValConParamClash nameRaw
        def
          & traverseOf
            _astTypeDefParameters
            ( maybe (throwError $ ParamNotFound old) pure
                . findAndAdjust ((== old) . fst) (_1 .~ new)
            )
      updateConstructors =
        traverseOf
          ( _astTypeDefConstructors
              % traversed
              % _valConArgs
              % traversed
          )
          $ traverseOf _freeVarsTy
          $ \(_, v) -> tvar $ updateName v
      updateName n = if n == old then new else n
  AddCon type_ index (unsafeMkGlobalName . (fmap unName (unModuleName (qualifiedModule type_)),) -> con) ->
    editModuleSameSelectionCross (qualifiedModule type_) prog $ \(m, ms) -> do
      when (con `elem` allValConNames prog) $ throwError $ ConAlreadyExists con
      m' <- updateType m
      traverseOf
        (traversed % _moduleDefs % traversed % _DefAST % _astDefExpr)
        updateDefs
        $ m' : ms
    where
      updateDefs = transformCaseBranches prog type_ $ \bs -> do
        m' <- DSL.meta
        maybe (throwError $ IndexOutOfRange index) pure $ insertAt index (CaseBranch con [] (EmptyHole m')) bs
      updateType =
        alterTypeDef
          ( traverseOf
              _astTypeDefConstructors
              (maybe (throwError $ IndexOutOfRange index) pure . insertAt index (ValCon con []))
          )
          type_
  SetConFieldType type_ con index new ->
    editModuleSameSelectionCross (qualifiedModule type_) prog $ \(m, ms) -> do
      m' <- updateType m
      traverseOf (traversed % _moduleDefs) updateDefs (m' : ms)
    where
      updateType =
        alterTypeDef
          ( traverseOf _astTypeDefConstructors $
              maybe (throwError $ ConNotFound con) pure
                <=< findAndAdjustA
                  ((== con) . valConName)
                  ( traverseOf
                      _valConArgs
                      ( maybe (throwError $ IndexOutOfRange index) pure
                          <=< adjustAtA index (const $ generateTypeIDs new)
                      )
                  )
          )
          type_
      updateDefs = traverseOf (traversed % _DefAST % _astDefExpr) (updateDecons <=< updateCons)
      updateCons e = case unfoldApp e of
        (h, args) -> case unfoldAPP h of
          (Con _ con', _tyArgs) | con' == con -> do
            let typecache = _typecache % _Just
            -- Previously the @index@th argument @t@ to this
            -- constructor would have been typechecked against the old
            -- field type @S@, @S ∋ t@.
            -- With the new field type @T@, we need to change @t@ to
            -- @t'@ such that @T ∋ t'@, which we do by:
            -- - if @t@ is a hole, set @t'=t@
            -- - if @t@ were synthesisable, set @t' = {? t ?}@
            -- - if @t@ were only checkable, set @t' = {? t : S ?}@
            -- Note that we make these choices because the contents
            -- of a non-empty hole must be synthesisable (but we
            -- don't care what particular type it synthesises).
            -- Note also that we assume the metadata (typecache) is up
            -- to date or blank, and we ensure this is the case in our
            -- output.
            -- We must work here to ensure that the result is
            -- well-typed, since we wish to avoid rechecking the whole
            -- program just to fix it up using smartholes.
            let enhole x = case (x, x ^? typecache % _synthed, x ^? typecache % _chkedAt) of
                  (EmptyHole{}, _, _) -> pure x
                  (Hole{}, _, _) -> pure x
                  (_, Just s, _) -> hole $ pure $ x & typecache .~ TCSynthed s
                  (_, Nothing, Just c) -> hole $ pure x `ann` generateTypeIDs c
                  -- This last case means that the input program had no (useful) metadata
                  (_, Nothing, Nothing) -> hole $ pure x `ann` tEmptyHole
            adjustAtA index enhole args >>= \case
              Just args' -> foldApp h =<< traverse (descendM updateCons) args'
              Nothing -> do
                -- The constructor is not applied as far as the changed field,
                -- so the full application still typechecks, but its type has changed.
                -- Thus, we put the whole thing in to a hole.
                Hole <$> DSL.meta <*> (foldApp h =<< traverse (descendM updateCons) args)
          _ ->
            -- NB we can't use `transformM` here because we'd end up seeing incomplete applications before full ones
            descendM updateCons e
      updateDecons = transformCaseBranches prog type_ $
        traverse $ \cb@(CaseBranch vc binds e) ->
          if vc == con
            then do
              Bind _ v <- maybe (throwError $ IndexOutOfRange index) pure $ binds !? index
              CaseBranch vc binds
                <$>
                -- TODO a custom traversal could be more efficient - reusing `_freeTmVars` means that we continue in
                -- to parts of the tree where `v` is shadowed, and thus where the traversal will never have any effect
                traverseOf
                  _freeTmVars
                  ( \(m, v') ->
                      if v' == v
                        then Hole <$> DSL.meta <*> pure (Var m $ LocalVarRef v')
                        else pure (Var m $ LocalVarRef v')
                  )
                  e
            else pure cb
  AddConField type_ con index new ->
    editModuleSameSelectionCross (qualifiedModule type_) prog $ \(m, ms) -> do
      m' <- updateType m
      traverseOf (traversed % _moduleDefs) updateDefs (m' : ms)
    where
      updateType =
        alterTypeDef
          ( traverseOf _astTypeDefConstructors $
              maybe (throwError $ ConNotFound con) pure
                <=< findAndAdjustA
                  ((== con) . valConName)
                  ( traverseOf
                      _valConArgs
                      ( maybe (throwError $ IndexOutOfRange index) pure
                          <=< liftA2 (insertAt index) (generateTypeIDs new) . pure
                      )
                  )
          )
          type_
      -- NB: we must updateDecons first, as transformCaseBranches may do
      -- synthesis of the scrutinee's type, using the old typedef. Thus we must
      -- not update the scrutinee before this happens.
      updateDefs = traverseOf (traversed % _DefAST % _astDefExpr) (updateCons <=< updateDecons)
      updateCons e = case unfoldApp e of
        (h, args) -> case unfoldAPP h of
          (Con _ con', _tyArgs) | con' == con -> do
            m' <- DSL.meta
            case insertAt index (EmptyHole m') args of
              Just args' -> foldApp h =<< traverse (descendM updateCons) args'
              Nothing ->
                -- The constructor is not applied as far as the field immediately prior to the new one,
                -- so the full application still typechecks, but its type has changed.
                -- Thus, we put the whole thing in to a hole.
                Hole <$> DSL.meta <*> (foldApp h =<< traverse (descendM updateCons) args)
          _ ->
            -- NB we can't use `transformM` here because we'd end up seeing incomplete applications before full ones
            descendM updateCons e
      updateDecons = transformCaseBranches prog type_ $
        traverse $ \cb@(CaseBranch vc binds e) ->
          if vc == con
            then do
              m' <- DSL.meta
              newName <- LocalName <$> freshName (freeVars e)
              binds' <- maybe (throwError $ IndexOutOfRange index) pure $ insertAt index (Bind m' newName) binds
              pure $ CaseBranch vc binds' e
            else pure cb
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
    res <- applyActionsToTypeSig smartHoles (progImports prog) ms (defName, def) actions
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
  SetSmartHoles smartHoles ->
    pure $ prog & _progSmartHoles .~ smartHoles
  CopyPasteSig fromIds setup -> case mdefName of
    Nothing -> throwError NoDefSelected
    Just i -> copyPasteSig prog fromIds i setup
  CopyPasteBody fromIds setup -> case mdefName of
    Nothing -> throwError NoDefSelected
    Just i -> copyPasteBody prog fromIds i setup
  RenameModule oldName newName -> do
    -- Call editModuleSameSelection solely for checking that 'oldName'
    -- is an editable module
    _ <- editModuleSameSelection oldName prog pure
    let n = ModuleName $ unsafeMkName <$> newName
        curMods = RM{imported = progImports prog, editable = progModules prog}
     in if n == oldName
          then pure prog
          else case renameModule oldName n curMods of
            Nothing -> throwError RenameModuleNameClash
            Just renamedMods ->
              if imported curMods == imported renamedMods
                then
                  pure $
                    prog
                      & _progModules .~ editable renamedMods
                      & _progSelection % _Just %~ renameModule' oldName n
                else
                  throwError $
                    -- It should never happen that the action edits an
                    -- imported module, since the oldName should be distinct
                    -- from the name of any import
                    ActionError $
                      InternalFailure "RenameModule: imported modules were edited by renaming"

-- Helper for RenameModule action
data RenameMods a = RM {imported :: [a], editable :: [a]}
  deriving stock (Functor, Foldable, Traversable)

lookupEditableModule :: MonadError ProgError m => ModuleName -> Prog -> m Module
lookupEditableModule n p =
  lookupModule' n p >>= \case
    MLEditable m -> pure m
    MLImported _ -> throwError $ ModuleReadonly n

-- | Describes return type of successfully looking a module up in the program.
-- We get the module and also whether it is imported or not.
data ModuleLookup = MLEditable Module | MLImported Module

lookupModule' :: MonadError ProgError m => ModuleName -> Prog -> m ModuleLookup
lookupModule' n p = case (find ((n ==) . moduleName) (progModules p), find ((n ==) . moduleName) (progImports p)) of
  (Just m, _) -> pure $ MLEditable m
  (Nothing, Just m) -> pure $ MLImported m
  (Nothing, Nothing) -> throwError $ ModuleNotFound n

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

editModuleSameSelection ::
  MonadError ProgError m =>
  ModuleName ->
  Prog ->
  (Module -> m Module) ->
  m Prog
editModuleSameSelection n p f = editModule n p (fmap (,progSelection p) . f)

-- A variant of 'editModuleSameSelection' for actions which can affect multiple modules
editModuleSameSelectionCross ::
  MonadError ProgError m =>
  ModuleName ->
  Prog ->
  ((Module, [Module]) -> m [Module]) ->
  m Prog
editModuleSameSelectionCross n p f = editModuleCross n p (fmap (,progSelection p) . f)

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
    (a : as) -> do
      runEditAppM (replay (reverse as)) start >>= \case
        (Right _, app') -> do
          put $ app' & _currentState % _prog % _redoLog .~ push a (redoLog prog)
          gets appProg
        (Left err, _) -> throwError err

-- | Redo the last undo by replaying it from the current program
-- state.
--
-- If the replay is successful, then we return the new program and
-- pop the last undo off the redo log.
--
-- If the replay is unsuccessful, then we throw a 'ProgError' and
-- leave it to the caller to decide how to handle it.
--
-- If the redo log is empty, return the program unchanged.
handleRedoRequest :: (MonadEditApp l ProgError m) => m Prog
handleRedoRequest = do
  prog <- gets appProg
  app <- get
  case pop (redoLog prog) of
    Nothing -> pure prog
    Just (a, redoLog') -> do
      runEditAppM (handleEditRequest a) app >>= \case
        (Right _, app') -> do
          put $ app' & _currentState % _prog % _redoLog .~ redoLog'
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
type MonadEditApp l e m = (MonadLog (WithSeverity l) m, MonadEdit m e, MonadState App m)

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
  deriving newtype (Functor, Applicative, Monad, MonadState App, MonadError e, MonadLog l)

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

-- | Construct a new, empty expression
newExpr :: MonadFresh ID m => m Expr
newExpr = do
  id_ <- fresh
  pure $ EmptyHole (Meta id_ Nothing Nothing)

-- | Construct a new, empty type
newType :: MonadFresh ID m => m Type
newType = do
  id_ <- fresh
  pure $ TEmptyHole (Meta id_ Nothing Nothing)

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
    doneSetup <- applyActionsToTypeSig smartHoles (progImports p) (mod, otherModules) (toDefBaseName, oldDef) setup
    tgt <- case doneSetup of
      Left err -> throwError $ ActionError err
      Right (_, tgt) -> pure $ focusOnlyType tgt
    let sharedScope =
          if fromDefName == toDefName
            then getSharedScopeTy c $ Right tgt
            else mempty
    -- Delete unbound vars
    let cTgt = either target target c
        f (m, n) =
          if Set.member (unLocalName n) sharedScope
            then pure $ TVar m n
            else fresh <&> \i -> TEmptyHole (Meta i Nothing Nothing)
    cScoped <- traverseOf _freeVarsTy f cTgt
    freshCopy <- regenerateTypeIDs cScoped
    pasted <- case target tgt of
      TEmptyHole _ -> pure $ replace freshCopy tgt
      _ -> throwError $ CopyPasteError "copy/paste setup didn't select an empty hole"
    let newDef = oldDef{astDefType = fromZipper pasted}
    let newSel = NodeSelection SigNode (pasted ^. _target % _typeMetaLens % re _Right)
    pure (insertDef mod toDefBaseName (DefAST newDef), Just (Selection toDefName $ Just newSel))
  liftError ActionError $ tcWholeProg finalProg

-- We cannot use bindersAbove as that works on names only, and different scopes
-- may reuse the same names. However, we want to detect that as non-shared.
-- Instead, we rely on fact that IDs are unique.
-- NB: we assume that both arguments belong in the same definition (and thus
-- only require that IDs are unique within one definition -- this is a narrower
-- uniqueness assumption than we currently enforce, see Note [Modules]).
getSharedScopeTy :: Either TypeZ TypeZip -> Either TypeZ TypeZip -> Set.Set Name
getSharedScopeTy = getSharedScope' bindersLocAbove
  where
    bindersLocAbove = either bindersLocAboveTypeZ bindersLocAboveTy
    bindersLocAboveTy :: TypeZip -> Set.Set (ID, Name)
    bindersLocAboveTy = foldAbove (\t -> Set.map ((getID $ current t,) . unLocalName) $ getBoundHereUpTy t)
    bindersLocAboveTypeZ :: TypeZ -> Set.Set (ID, Name)
    bindersLocAboveTypeZ =
      foldAboveTypeZ
        (\t -> Set.map ((getID $ current t,) . unLocalName) $ getBoundHereUpTy t)
        ((\e -> Set.map (getID e,) $ getBoundHere e Nothing) . current)
        (\x -> Set.map (getID $ current x,) $ getBoundHereUp x)

-- TODO: there is a lot of duplicated code for copy/paste, often due to types/terms being different...
getSharedScope :: ExprZ -> ExprZ -> Set.Set Name
getSharedScope = getSharedScope' bindersLocAbove
  where
    bindersLocAbove :: ExprZ -> Set.Set (ID, Name)
    bindersLocAbove = foldAbove (\x -> Set.map (getID $ current x,) $ getBoundHereUp x)

getSharedScope' :: (a -> Set.Set (ID, Name)) -> a -> a -> Set.Set Name
getSharedScope' bindersLocAbove l r =
  let lBinds = bindersLocAbove l
      rBinds = bindersLocAbove r
      common = names $ Set.intersection lBinds rBinds
      onlyLeft = names $ lBinds Set.\\ rBinds
      onlyRight = names $ rBinds Set.\\ lBinds
   in -- NB: 'names' is not injective, thus there can be elements in
      -- both 'common' and 'onlyLeft'/'onlyRight'
      -- We must filter these out since any reference to such a name
      -- would either escape its scope (left) or be captured (right)
      (common Set.\\ onlyLeft) Set.\\ onlyRight
  where
    names :: Set.Set (ID, Name) -> Set.Set Name
    names = Set.map snd

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
        { trusted = progImports p
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
tcWholeProgWithImports p = do
  imports <- checkEverything (progSmartHoles p) CheckEverything{trusted = mempty, toCheck = progImports p}
  tcWholeProg $ p & _progImports .~ imports

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
        let sharedScope =
              if fromDefName == toDefName
                then getSharedScopeTy srcT $ Left tgtT
                else mempty
        -- Delete unbound vars. TODO: we may want to let-bind them?
        let srcSubtree = either target target srcT
            f (m, n) =
              if Set.member (unLocalName n) sharedScope
                then pure $ TVar m n
                else fresh <&> \i -> TEmptyHole (Meta i Nothing Nothing)
        scopedCopy <- traverseOf _freeVarsTy f srcSubtree
        freshCopy <- regenerateTypeIDs scopedCopy
        pasted <- case target tgtT of
          TEmptyHole _ -> pure $ replace freshCopy tgtT
          _ -> throwError $ CopyPasteError "copy/paste setup didn't select an empty hole"
        let newDef = oldDef{astDefExpr = unfocusExpr $ unfocusType pasted}
        let newSel = NodeSelection BodyNode (pasted ^. _target % _typeMetaLens % re _Right)
        pure (insertDef mod toDefBaseName (DefAST newDef), Just (Selection toDefName $ Just newSel))
      (Left srcE, InExpr tgtE) -> do
        let sharedScope =
              if fromDefName == toDefName
                then getSharedScope srcE tgtE
                else mempty
        -- Delete unbound vars. TODO: we may want to let-bind them?
        let tm (m, n) =
              if Set.member (unLocalName n) sharedScope
                then pure $ Var m $ LocalVarRef n
                else fresh <&> \i -> EmptyHole (Meta i Nothing Nothing)
            ty (m, n) =
              if Set.member (unLocalName n) sharedScope
                then pure $ TVar m n
                else fresh <&> \i -> TEmptyHole (Meta i Nothing Nothing)
        scopedCopy <- traverseOf _freeTyVars ty =<< traverseOf _freeTmVars tm (target srcE)
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

alterTypeDef ::
  MonadError ProgError m =>
  (ASTTypeDef TypeMeta -> m (ASTTypeDef TypeMeta)) ->
  TyConName ->
  Module ->
  m Module
alterTypeDef f type_ m = do
  unless (qualifiedModule type_ == moduleName m) $ throwError $ TypeDefNotFound type_
  traverseOf
    _moduleTypes
    ( Map.alterF
        ( maybe
            (throwError $ TypeDefNotFound type_)
            ( maybe
                (throwError $ TypeDefIsPrim type_)
                (map (Just . TypeDefAST) . f)
                . typeDefAST
            )
        )
        (baseName type_)
    )
    m

-- | Apply a bottom-up transformation to all branches of case expressions on the given type.
transformCaseBranches ::
  MonadEdit m ProgError =>
  Prog ->
  TyConName ->
  ([CaseBranch] -> m [CaseBranch]) ->
  Expr ->
  m Expr
transformCaseBranches prog type_ f = transformM $ \case
  Case m scrut bs -> do
    scrutType <-
      fst
        <$> runReaderT
          (liftError (ActionError . TypeError) $ synth scrut)
          (progCxt prog)
    Case m scrut
      <$> if fst (unfoldTApp scrutType) == TCon () type_
        then f bs
        else pure bs
  e -> pure e

progCxt :: Prog -> Cxt
progCxt p = buildTypingContextFromModules (progAllModules p) (progSmartHoles p)

-- | Run a computation in some context whose errors can be promoted to `ProgError`.
liftError :: MonadError ProgError m => (e -> ProgError) -> ExceptT e m b -> m b
liftError = modifyError

allConNames :: Prog -> [(TyConName, [ValConName])]
allConNames p = ifoldMap (\tn td -> pure (tn, valConName <$> typeDefConstructors td)) $ allTypes p
  where
    typeDefConstructors td = maybe [] astTypeDefConstructors $ typeDefAST td

allValConNames :: Prog -> [ValConName]
allValConNames = snd <=< allConNames

allTyConNames :: Prog -> [TyConName]
allTyConNames = fmap fst . allConNames
