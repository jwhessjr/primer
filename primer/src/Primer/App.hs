{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

-- This module defines the high level application functions.

module Primer.App (
  Log (..),
  App (..),
  InitialApp (..),
  initialApp,
  newProg,
  newEmptyProg,
  newApp,
  newEmptyApp,
  EditAppM,
  QueryAppM,
  runEditAppM,
  runQueryAppM,
  Prog (..),
  progAllModules,
  tcWholeProg,
  ProgAction (..),
  ProgError (..),
  Question (..),
  handleQuestion,
  handleGetProgramRequest,
  handleMutationRequest,
  handleEditRequest,
  handleEvalRequest,
  handleEvalFullRequest,
  importModules,
  MutationRequest (..),
  Selection (..),
  NodeSelection (..),
  NodeType (..),
  EvalReq (..),
  EvalResp (..),
  EvalFullReq (..),
  EvalFullResp (..),
  lookupASTDef,
) where

import Foreword hiding (mod)

import Control.Monad.Fresh (MonadFresh (..))
import Data.Aeson (
  ToJSON (toEncoding),
  defaultOptions,
  genericToEncoding,
 )
import Data.Bitraversable (bimapM)
import Data.Generics.Product (position)
import Data.Generics.Uniplate.Operations (descendM, transform, transformM)
import Data.Generics.Uniplate.Zipper (
  fromZipper,
 )
import Data.List (intersect, (\\))
import Data.List.Extra (anySame, disjoint, (!?))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Optics (
  Field1 (_1),
  Field2 (_2),
  ReversibleOptic (re),
  over,
  toListOf,
  traverseOf,
  traversed,
  view,
  (%),
  (%~),
  (.~),
  (?~),
  (^.),
  _Left,
  _Right,
 )
import Primer.Action (
  Action,
  ActionError (..),
  ProgAction (..),
  applyActionsToBody,
  applyActionsToTypeSig,
 )
import Primer.Builtins (builtinModule)
import Primer.Core (
  ASTDef (..),
  ASTTypeDef (..),
  Bind' (Bind),
  CaseBranch,
  CaseBranch' (CaseBranch),
  Def (..),
  Expr,
  Expr' (Case, Con, EmptyHole, Hole, Var),
  ExprMeta,
  GVarName,
  GlobalName (baseName, qualifiedModule),
  ID (..),
  LocalName (LocalName, unLocalName),
  Meta (..),
  ModuleName,
  TmVarRef (GlobalVarRef, LocalVarRef),
  TyConName,
  TyVarName,
  Type,
  Type' (..),
  TypeDef (..),
  TypeMeta,
  ValCon (..),
  ValConName,
  defAST,
  defName,
  defPrim,
  getID,
  qualifyName,
  typeDefAST,
  typesInExpr,
  unsafeMkGlobalName,
  unsafeMkLocalName,
  _exprMeta,
  _exprMetaLens,
  _exprTypeMeta,
  _id,
  _typeMeta,
  _typeMetaLens,
 )
import Primer.Core.DSL (create, emptyHole, tEmptyHole)
import qualified Primer.Core.DSL as DSL
import Primer.Core.Transform (foldApp, renameVar, unfoldApp, unfoldTApp)
import Primer.Core.Utils (freeVars, _freeTmVars, _freeTyVars, _freeVarsTy)
import Primer.Eval (EvalDetail, EvalError)
import qualified Primer.Eval as Eval
import Primer.EvalFull (Dir, EvalFullError (TimedOut), TerminationBound, evalFull)
import Primer.JSON
import Primer.Module (
  Module (Module, moduleDefs, moduleName, moduleTypes),
  deleteDef,
  insertDef,
  mkTypeDefMap,
  moduleDefsQualified,
  moduleTypesQualified,
 )
import Primer.Name (Name (unName), NameCounter, freshName, unsafeMkName)
import Primer.Primitives (primitiveModule)
import Primer.Questions (
  Question (..),
  generateNameExpr,
  generateNameTy,
  variablesInScopeExpr,
  variablesInScopeTy,
 )
import Primer.Typecheck (
  CheckEverythingRequest (CheckEverything, toCheck, trusted),
  Cxt,
  SmartHoles (NoSmartHoles, SmartHoles),
  TypeError,
  buildTypingContextFromModules,
  checkDef,
  checkEverything,
  checkTypeDefs,
  mkTypeDefMapQualified,
  synth,
 )
import Primer.Zipper (
  ExprZ,
  Loc (InBind, InExpr, InType),
  TypeZ,
  TypeZip,
  bindersAbove,
  bindersAboveTy,
  bindersAboveTypeZ,
  current,
  focus,
  focusOn,
  focusOnTy,
  focusOnlyType,
  foldAbove,
  getBoundHere,
  getBoundHereTy,
  locToEither,
  replace,
  target,
  unfocusExpr,
  unfocusType,
  up,
  _target,
 )

-- | The program state, shared between the frontend and backend
--  This is much more info than we need to send - for example we probably don't
--  need to send the log back and forth.
--  But for now, to keep things simple, that's how it works.
data Prog = Prog
  { progImports :: [Module]
  -- ^ Some immutable imported modules
  , progModule :: Module
  -- ^ The one editable "current" module
  , progSelection :: Maybe Selection
  , progSmartHoles :: SmartHoles
  , progLog :: Log -- The log of all actions
  }
  deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via VJSON Prog

progAllModules :: Prog -> [Module]
progAllModules p = progModule p : progImports p

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

-- | Imports some explicitly-given modules, ensuring that they are well-typed
-- (and all their dependencies are already imported)
importModules :: MonadEditApp m => [Module] -> m ()
importModules ms = do
  p <- gets appProg
  -- Module names must be unique
  let currentModules = progAllModules p
  let currentNames = moduleName <$> currentModules
  let newNames = moduleName <$> ms
  unless (disjoint currentNames newNames && not (anySame newNames)) $
    throwError $
      ActionError $
        ImportNameClash $
          (currentNames `intersect` newNames) <> (newNames \\ ordNub newNames)
  -- Imports must be well-typed (and cannot depend on the editable module)
  checkedImports <-
    liftError (ActionError . ImportFailed ()) $
      checkEverything NoSmartHoles $
        CheckEverything{trusted = progImports p, toCheck = ms}
  let p' = p & #progImports %~ (<> checkedImports)
  modify (\a -> a{appProg = p'})

-- | Get all type definitions from all modules (including imports)
allTypes :: Prog -> Map TyConName TypeDef
allTypes p = foldMap moduleTypesQualified $ progAllModules p

-- | Get all definitions from all modules (including imports)
allDefs :: Prog -> Map GVarName Def
allDefs p = foldMap moduleDefsQualified $ progAllModules p

-- | Add a definition to the editable module
-- assumes the def has the correct name to go in the editable module
addDef :: ASTDef -> Prog -> Prog
addDef d p =
  let mod = progModule p
      mod' = insertDef mod $ DefAST d
   in p{progModule = mod'}

-- | Add a type definition to the editable module
-- assumes the def has the correct name to go in the editable module
addTypeDef :: ASTTypeDef -> Prog -> Prog
addTypeDef t p =
  let mod = progModule p
      tydefs = moduleTypes mod
      tydefs' = tydefs <> mkTypeDefMap [TypeDefAST t]
      mod' = mod{moduleTypes = tydefs'}
   in p{progModule = mod'}

-- | The action log
--  This is the canonical store of the program - we can recreate any current or
--  past program state by replaying this log.
--  Each item is a sequence of Core actions which should be applied atomically.
--  Items are stored in reverse order so it's quick to add new ones.
newtype Log = Log {unlog :: [[ProgAction]]}
  deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via VJSON Log

-- | Describes what interface element the user has selected.
-- A definition in the left hand nav bar, and possibly a node in that definition.
data Selection = Selection
  { selectedDef :: GVarName
  -- ^ the ID of some ASTDef
  , selectedNode :: Maybe NodeSelection
  }
  deriving (Eq, Show, Generic)
  deriving (ToJSON, FromJSON) via VJSON Selection

-- | A selected node, in the body or type signature of some definition.
-- We have the following invariant: @nodeType = SigNode ==> isRight meta@
data NodeSelection = NodeSelection
  { nodeType :: NodeType
  , nodeId :: ID
  , meta :: Either ExprMeta TypeMeta
  }
  deriving (Eq, Show, Generic)
  deriving (ToJSON, FromJSON) via VJSON NodeSelection

data NodeType = BodyNode | SigNode
  deriving (Eq, Show, Generic)
  deriving (ToJSON, FromJSON) via VJSON NodeType

-- | The type of requests which can mutate the application state.
--
-- Note that `Reset` is not undo-able, as it wipes the log along with
-- all other program state, IDs, etc.
data MutationRequest
  = Undo
  | Reset
  | Edit [ProgAction]
  deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via VJSON MutationRequest

data ProgError
  = NoDefSelected
  | DefNotFound GVarName
  | DefAlreadyExists GVarName
  | DefInUse GVarName
  | TypeDefIsPrim TyConName
  | TypeDefNotFound TyConName
  | TypeDefAlreadyExists TyConName
  | ConNotFound ValConName
  | ConAlreadyExists ValConName
  | ParamNotFound TyVarName
  | ParamAlreadyExists TyVarName
  | TyConParamClash Name
  | ValConParamClash Name
  | ActionError ActionError
  | EvalError EvalError
  | -- | Currently copy/paste is only exposed in the frontend via select
    --   channels, which should never go wrong. Consequently, this is an
    --   "internal error" which should never happen!
    --   If/when we expose it more broadly, we should refactor this to contain
    --   a descriptive ADT, rather than a string.
    CopyPasteError Text
  | -- | Currently one can only add a typedef by a form in the frontend,
    --   which does its own error checking. Thus this is an "internal error"
    --   that should never happen!
    --   (However, this is not entirely true currently, see
    --    https://github.com/hackworthltd/primer/issues/3)
    TypeDefError Text
  | IndexOutOfRange Int
  deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via VJSON ProgError

data EvalReq = EvalReq
  { evalReqExpr :: Expr
  , evalReqRedex :: ID
  }
  deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via VJSON EvalReq

data EvalResp = EvalResp
  { evalRespExpr :: Expr
  , evalRespRedexes :: [ID]
  , evalRespDetail :: EvalDetail
  }
  deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via VJSON EvalResp

data EvalFullReq = EvalFullReq
  { evalFullReqExpr :: Expr
  , evalFullCxtDir :: Dir -- is this expression in a syn/chk context, so we can tell if is an embedding.
  , evalFullMaxSteps :: TerminationBound
  }
  deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via VJSON EvalFullReq

-- If we time out, we still return however far we got
data EvalFullResp
  = EvalFullRespTimedOut Expr
  | EvalFullRespNormal Expr
  deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via VJSON EvalFullResp

-- * Request handlers

-- | Handle a question
-- Note that these only consider the non-imported module as a location of which
-- to ask a question. However, they will return variables which are in scope by
-- dint of being imported.
handleQuestion :: MonadQueryApp m => Question a -> m a
handleQuestion = \case
  VariablesInScope defid exprid -> do
    node <- focusNode' defid exprid
    defs <- asks $ allDefs . appProg
    let (tyvars, termvars, globals) = case node of
          Left zE -> variablesInScopeExpr defs zE
          Right zT -> (variablesInScopeTy zT, [], [])
    pure ((tyvars, termvars), globals)
  GenerateName defid nodeid typeKind -> do
    prog <- asks appProg
    names <-
      focusNode' defid nodeid <&> \case
        Left zE -> generateNameExpr typeKind zE
        Right zT -> generateNameTy typeKind zT
    pure $ runReader names $ progCxt prog
  where
    focusNode' defname nodeid = do
      prog <- asks appProg
      focusNode prog defname nodeid

-- This only looks in the editable module, not in any imports
focusNode :: MonadError ProgError m => Prog -> GVarName -> ID -> m (Either (Either ExprZ TypeZ) TypeZip)
focusNode prog = focusNodeDefs $ moduleDefsQualified $ progModule prog

-- This looks in the editable module and also in any imports
focusNodeImports :: MonadError ProgError m => Prog -> GVarName -> ID -> m (Either (Either ExprZ TypeZ) TypeZip)
focusNodeImports prog = focusNodeDefs $ allDefs prog

focusNodeDefs :: MonadError ProgError m => Map GVarName Def -> GVarName -> ID -> m (Either (Either ExprZ TypeZ) TypeZip)
focusNodeDefs defs defname nodeid =
  case lookupASTDef defname defs of
    Nothing -> throwError $ DefNotFound defname
    Just def ->
      let mzE = locToEither <$> focusOn nodeid (focus $ astDefExpr def)
          mzT = focusOnTy nodeid $ focus $ astDefType def
       in case fmap Left mzE <|> fmap Right mzT of
            Nothing -> throwError $ ActionError (IDNotFound nodeid)
            Just x -> pure x

-- | Handle a request to retrieve the current program
handleGetProgramRequest :: MonadReader App m => m Prog
handleGetProgramRequest = asks appProg

-- | Handle a request to mutate the app state
handleMutationRequest :: MonadEditApp m => MutationRequest -> m Prog
handleMutationRequest = \case
  Edit as -> handleEditRequest as
  Undo -> handleUndoRequest
  Reset -> handleResetRequest

-- | Handle an edit request
handleEditRequest :: forall m. MonadEditApp m => [ProgAction] -> m Prog
handleEditRequest actions = do
  (prog, _) <- gets appProg >>= \p -> foldM go (p, Nothing) actions
  let Log l = progLog prog
  let prog' = prog{progLog = Log (actions : l)}
  modify (\s -> s{appProg = prog'})
  pure prog'
  where
    go :: (Prog, Maybe GVarName) -> ProgAction -> m (Prog, Maybe GVarName)
    go (prog, mdef) = applyProgAction prog mdef

-- | Handle an eval request
handleEvalRequest :: MonadEditApp m => EvalReq -> m EvalResp
handleEvalRequest req = do
  prog <- gets appProg
  result <- Eval.step (allDefs prog) (evalReqExpr req) (evalReqRedex req)
  case result of
    Left err -> throwError $ EvalError err
    Right (expr, detail) ->
      pure
        EvalResp
          { evalRespExpr = expr
          , evalRespRedexes = Set.toList $ Eval.redexes (Map.mapMaybe defPrim $ allDefs prog) expr
          , evalRespDetail = detail
          }

-- | Handle an eval-to-normal-form request
handleEvalFullRequest :: MonadEditApp m => EvalFullReq -> m EvalFullResp
handleEvalFullRequest (EvalFullReq{evalFullReqExpr, evalFullCxtDir, evalFullMaxSteps}) = do
  prog <- gets appProg
  result <- evalFull (allTypes prog) (allDefs prog) evalFullMaxSteps evalFullCxtDir evalFullReqExpr
  pure $ case result of
    Left (TimedOut e) -> EvalFullRespTimedOut e
    Right nf -> EvalFullRespNormal nf

-- Prog actions only affect the editable module
applyProgAction :: MonadEditApp m => Prog -> Maybe GVarName -> ProgAction -> m (Prog, Maybe GVarName)
applyProgAction prog mdefName = \case
  MoveToDef d -> case Map.lookup d (moduleDefsQualified $ progModule prog) of
    Nothing -> throwError $ DefNotFound d
    Just _ -> pure (prog, Just d)
  DeleteDef d ->
    case deleteDef (progModule prog) d of
      Nothing -> throwError $ DefNotFound d
      Just mod' -> do
        let prog' = prog{progModule = mod', progSelection = Nothing}
        -- Run a full TC solely to ensure that no references to the removed id
        -- remain. This is rather inefficient and could be improved in the
        -- future.
        void . liftError (const $ DefInUse d) $
          checkEverything @TypeError
            NoSmartHoles
            CheckEverything{trusted = progImports prog, toCheck = [mod']}
        pure (prog', Nothing)
  RenameDef d nameStr -> case lookupASTDef d (moduleDefsQualified $ progModule prog) of
    Nothing -> throwError $ DefNotFound d
    Just def -> do
      let defs = moduleDefs $ progModule prog
          oldNameBase = baseName d
          newNameBase = unsafeMkName nameStr
          newName = qualifyName (moduleName $ progModule prog) newNameBase
      if Map.member newNameBase defs
        then throwError $ DefAlreadyExists newName
        else do
          let def' = DefAST def{astDefName = newName}
          defs' <-
            maybe (throwError $ ActionError NameCapture) pure $
              traverse
                ( traverseOf (#_DefAST % #astDefExpr) $
                    renameVar (GlobalVarRef d) (GlobalVarRef newName)
                )
                (Map.insert newNameBase def' $ Map.delete oldNameBase defs)
          let prog' =
                prog
                  & #progSelection ?~ Selection (defName def') Nothing
                  & #progModule % #moduleDefs .~ defs'
          pure (prog', mdefName)
  CreateDef n -> do
    let mod = progModule prog
    let modName = moduleName mod
    let defs = moduleDefs mod
    name <- case n of
      Just nameStr ->
        let baseName = unsafeMkName nameStr
            name = qualifyName modName baseName
         in if Map.member baseName defs
              then throwError $ DefAlreadyExists name
              else pure name
      Nothing -> fmap (qualifyName modName) $ freshName $ Map.keysSet defs
    expr <- newExpr
    ty <- newType
    let def = ASTDef name expr ty
    pure (addDef def prog{progSelection = Just $ Selection name Nothing}, Just name)
  AddTypeDef td -> do
    -- The frontend should never let this error happen,
    -- so we just dump out a raw string for debugging/logging purposes
    let m = moduleName $ progModule prog
    unless (m == qualifiedModule (astTypeDefName td)) $
      throwError $ TypeDefError $ "Cannot create a type definition with incorrect module name: expected " <> unName m
    (addTypeDef td prog, mdefName)
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
            (checkTypeDefs $ mkTypeDefMapQualified [TypeDefAST td])
            (buildTypingContextFromModules (progAllModules prog) NoSmartHoles)
        )
  RenameType old (unsafeMkName -> nameRaw) ->
    (,Nothing) <$> do
      traverseOf
        #progModule
        ( traverseOf #moduleTypes (updateType <=< pure . updateRefsInTypes)
            <=< pure . over (#moduleDefs % traversed % #_DefAST) (updateDefBody . updateDefType)
        )
        prog
    where
      new = qualifyName (qualifiedModule old) nameRaw
      updateType m = do
        d0 <-
          -- NB We do not allow primitive types to be renamed.
          -- To relax this, we'd have to be careful about how it interacts with type-checking of primitive literals.
          maybe (throwError $ TypeDefIsPrim old) pure . typeDefAST
            =<< maybe (throwError $ TypeDefNotFound old) pure (Map.lookup (baseName old) m)
        when (Map.member nameRaw m) $ throwError $ TypeDefAlreadyExists new
        when (nameRaw `elem` map (unLocalName . fst) (astTypeDefParameters d0)) $ throwError $ TyConParamClash nameRaw
        pure $ Map.insert nameRaw (TypeDefAST $ d0 & #astTypeDefName .~ new) $ Map.delete (baseName old) m
      updateRefsInTypes =
        over
          (traversed % #_TypeDefAST % #astTypeDefConstructors % traversed % #valConArgs % traversed)
          $ transform $ over (#_TCon % _2) updateName
      updateDefType =
        over
          #astDefType
          $ transform $ over (#_TCon % _2) updateName
      updateDefBody =
        over
          #astDefExpr
          $ transform $ over typesInExpr $ transform $ over (#_TCon % _2) updateName
      updateName n = if n == old then new else n
  RenameCon type_ old (unsafeMkGlobalName . (unName (qualifiedModule type_),) -> new) ->
    (,Nothing) <$> do
      when (new `elem` allConNames prog) $ throwError $ ConAlreadyExists new
      traverseOf
        #progModule
        ( updateType
            <=< traverseOf #moduleDefs (pure . updateDefs)
        )
        prog
    where
      updateType =
        alterTypeDef
          ( traverseOf
              #astTypeDefConstructors
              ( maybe (throwError $ ConNotFound old) pure
                  . findAndAdjust ((== old) . valConName) (#valConName .~ new)
              )
          )
          type_
      updateDefs =
        over (traversed % #_DefAST % #astDefExpr) $
          transform $ over (#_Con % _2) updateName
      updateName n = if n == old then new else n
  RenameTypeParam type_ old (unsafeMkLocalName -> new) -> do
    (,Nothing)
      <$> traverseOf
        #progModule
        updateType
        prog
    where
      updateType =
        alterTypeDef
          (pure . updateConstructors <=< updateParam)
          type_
      updateParam def = do
        when (new `elem` map fst (astTypeDefParameters def)) $ throwError $ ParamAlreadyExists new
        let nameRaw = unLocalName new
        when (nameRaw == baseName (astTypeDefName def)) $ throwError $ TyConParamClash nameRaw
        when (nameRaw `elem` map (baseName . valConName) (astTypeDefConstructors def)) $ throwError $ ValConParamClash nameRaw
        def
          & traverseOf
            #astTypeDefParameters
            ( maybe (throwError $ ParamNotFound old) pure
                . findAndAdjust ((== old) . fst) (_1 .~ new)
            )
      updateConstructors =
        over
          ( #astTypeDefConstructors
              % traversed
              % #valConArgs
              % traversed
          )
          $ over _freeVarsTy $ \(_, v) -> TVar () $ updateName v
      updateName n = if n == old then new else n
  AddCon type_ index (unsafeMkGlobalName . (unName (qualifiedModule type_),) -> con) ->
    (,Nothing)
      <$> do
        when (con `elem` allConNames prog) $ throwError $ ConAlreadyExists con
        traverseOf
          #progModule
          ( traverseOf
              (#moduleDefs % traversed % #_DefAST % #astDefExpr)
              updateDefs
              <=< updateType
          )
          prog
    where
      updateDefs = transformCaseBranches prog type_ $ \bs -> do
        m' <- DSL.meta
        maybe (throwError $ IndexOutOfRange index) pure $ insertAt index (CaseBranch con [] (EmptyHole m')) bs
      updateType =
        alterTypeDef
          ( traverseOf
              #astTypeDefConstructors
              (maybe (throwError $ IndexOutOfRange index) pure . insertAt index (ValCon con []))
          )
          type_
  SetConFieldType type_ con index new -> do
    (,Nothing)
      <$> traverseOf
        #progModule
        ( traverseOf #moduleDefs updateDefs
            <=< updateType
        )
        prog
    where
      updateType =
        alterTypeDef
          ( traverseOf #astTypeDefConstructors $
              maybe (throwError $ ConNotFound con) pure
                <=< findAndAdjustA
                  ((== con) . valConName)
                  ( traverseOf
                      #valConArgs
                      (maybe (throwError $ IndexOutOfRange index) pure . adjustAt index (const new))
                  )
          )
          type_
      updateDefs = traverseOf (traversed % #_DefAST % #astDefExpr) (updateDecons <=< updateCons)
      updateCons e = case unfoldApp e of
        (e'@(Con _ con'), args) | con' == con -> do
          m' <- DSL.meta
          case adjustAt index (Hole m') args of
            Just args' -> foldApp e' =<< traverse (descendM updateCons) args'
            Nothing -> do
              -- The constructor is not applied as far as the changed field,
              -- so the full application still typechecks, but its type has changed.
              -- Thus, we put the whole thing in to a hole.
              Hole <$> DSL.meta <*> (foldApp e' =<< traverse (descendM updateCons) args)
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
  AddConField type_ con index new -> do
    (,Nothing)
      <$> traverseOf
        #progModule
        ( traverseOf #moduleDefs updateDefs
            <=< updateType
        )
        prog
    where
      updateType =
        alterTypeDef
          ( traverseOf #astTypeDefConstructors $
              maybe (throwError $ ConNotFound con) pure
                <=< findAndAdjustA
                  ((== con) . valConName)
                  ( traverseOf
                      #valConArgs
                      (maybe (throwError $ IndexOutOfRange index) pure . insertAt index new)
                  )
          )
          type_
      updateDefs = traverseOf (traversed % #_DefAST % #astDefExpr) (updateDecons <=< updateCons)
      updateCons e = case unfoldApp e of
        (e'@(Con _ con'), args) | con' == con -> do
          m' <- DSL.meta
          case insertAt index (EmptyHole m') args of
            Just args' -> foldApp e' =<< traverse (descendM updateCons) args'
            Nothing ->
              -- The constructor is not applied as far as the field immediately prior to the new one,
              -- so the full application still typechecks, but its type has changed.
              -- Thus, we put the whole thing in to a hole.
              Hole <$> DSL.meta <*> (foldApp e' =<< traverse (descendM updateCons) args)
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
  BodyAction actions -> do
    withDef mdefName prog $ \def -> do
      smartHoles <- gets $ progSmartHoles . appProg
      res <- applyActionsToBody smartHoles (progAllModules prog) def actions
      case res of
        Left err -> throwError $ ActionError err
        Right (def', z) -> do
          let meta = bimap (view (position @1) . target) (view _typeMetaLens . target) $ locToEither z
              nodeId = either getID getID meta
          let prog' =
                addDef
                  def'
                  prog
                    { progSelection =
                        Just $
                          Selection (astDefName def') $
                            Just
                              NodeSelection
                                { nodeType = BodyNode
                                , nodeId
                                , meta
                                }
                    }
          pure (prog', mdefName)
  SigAction actions -> do
    withDef mdefName prog $ \def -> do
      smartHoles <- gets $ progSmartHoles . appProg
      res <- applyActionsToTypeSig smartHoles (progImports prog) (progModule prog) def actions
      case res of
        Left err -> throwError $ ActionError err
        Right (def', mod', zt) -> do
          let node = target zt
              meta = view _typeMetaLens node
              nodeId = getID meta
              prog' =
                prog
                  { progModule = mod'
                  , progSelection =
                      Just $
                        Selection (astDefName def') $
                          Just
                            NodeSelection
                              { nodeType = SigNode
                              , nodeId
                              , meta = Right meta
                              }
                  }
           in pure (prog', mdefName)
  SetSmartHoles smartHoles ->
    pure
      ( prog & #progSmartHoles .~ smartHoles
      , mdefName
      )
  CopyPasteSig fromIds setup -> case mdefName of
    Nothing -> throwError NoDefSelected
    Just i -> (,mdefName) <$> copyPasteSig prog fromIds i setup
  CopyPasteBody fromIds setup -> case mdefName of
    Nothing -> throwError NoDefSelected
    Just i -> (,mdefName) <$> copyPasteBody prog fromIds i setup

-- Look up the definition by its given Name, then run the given action with it
-- only looks in the editable module
withDef :: MonadEditApp m => Maybe GVarName -> Prog -> (ASTDef -> m a) -> m a
withDef mdefName prog f =
  case mdefName of
    Nothing -> throwError NoDefSelected
    Just defname -> do
      case lookupASTDef defname (moduleDefsQualified $ progModule prog) of
        Nothing -> throwError $ DefNotFound defname
        Just def -> f def

-- | Undo the last block of actions.
-- If there are no actions in the log we return the program unchanged.
-- We undo by replaying the whole log from the start.
-- Because actions often refer to the IDs of nodes created by previous actions
-- we must reset the ID and name counter to their original state before we
-- replay. We do this by resetting the entire app state.
handleUndoRequest :: MonadEditApp m => m Prog
handleUndoRequest = do
  prog <- gets appProg
  start <- gets (initialApp . appInit)
  case unlog (progLog prog) of
    [] -> pure prog
    (_ : as) -> do
      case runEditAppM (replay (reverse as)) start of
        (Right _, app') -> do
          put app'
          gets appProg
        (Left err, _) -> throwError err

-- Replay a series of actions, updating the app state with the new program
replay :: MonadEditApp m => [[ProgAction]] -> m ()
replay = mapM_ handleEditRequest

-- | Reset the entire program state, including any IDs that have been
-- generated.
--
-- This request cannot be undone!
handleResetRequest :: MonadEditApp m => m Prog
handleResetRequest = do
  app <- gets (initialApp . appInit)
  put app
  pure $ appProg app

-- | A shorthand for the constraints we need when performing mutation
-- operations on the application.
--
-- Note we do not want @MonadFresh Name m@, as @fresh :: m Name@ has
-- no way of avoiding user-specified names. Instead, use 'freshName'.
type MonadEditApp m = (Monad m, MonadFresh ID m, MonadFresh NameCounter m, MonadState App m, MonadError ProgError m)

-- | A shorthand for the constraints we need when performing read-only
-- operations on the application.
type MonadQueryApp m = (Monad m, MonadReader App m, MonadError ProgError m)

-- | The 'EditApp' monad.
--
-- Actions run in this monad can modify the 'App'. 'ExceptT' wraps
-- state so that an action that throws an error does not modify the
-- state. This is important to ensure that we can reliably replay the
-- log without having ID mismatches.
newtype EditAppM a = EditAppM (StateT App (Except ProgError) a)
  deriving newtype (Functor, Applicative, Monad, MonadState App, MonadError ProgError)

-- | Run an 'EditAppM' action, returning a result and an updated
-- 'App'.
runEditAppM :: EditAppM a -> App -> (Either ProgError a, App)
runEditAppM (EditAppM m) appState = case runExcept (runStateT m appState) of
  Left err -> (Left err, appState)
  Right (res, appState') -> (Right res, appState')

-- | The 'QueryApp' monad.
--
-- Actions run in this monad cannot modify the 'App'. We use 'ExceptT'
-- here for compatibility with 'EditApp'.
newtype QueryAppM a = QueryAppM (ReaderT App (Except ProgError) a)
  deriving newtype (Functor, Applicative, Monad, MonadReader App, MonadError ProgError)

-- | Run a 'QueryAppM' action, returning a result.
runQueryAppM :: QueryAppM a -> App -> Either ProgError a
runQueryAppM (QueryAppM m) appState = case runExcept (runReaderT m appState) of
  Left err -> Left err
  Right res -> Right res

-- | We use this type to remember which "new app" was used to
-- initialize the session. We need this so that program resets and
-- undo know which baseline app to start with when performing their
-- corresponding action.
data InitialApp
  = NewApp
  | NewEmptyApp
  deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via VJSON InitialApp

-- | Given an 'InitialApp', return the corresponding new app instance.
initialApp :: InitialApp -> App
initialApp NewApp = newApp
initialApp NewEmptyApp = newEmptyApp

-- | The global App state
--
-- Note that the 'ToJSON' and 'FromJSON' instances for this type are
-- not used in the frontend, and therefore we can use "Data.Aeson"s
-- generic instances for them.
data App = App
  { appIdCounter :: Int
  , appNameCounter :: NameCounter
  , appProg :: Prog
  , appInit :: InitialApp
  }
  deriving (Eq, Show, Generic)

instance ToJSON App where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON App

-- | An empty initial program.
newEmptyProg :: Prog
newEmptyProg =
  let expr = EmptyHole (Meta 1 Nothing Nothing)
      ty = TEmptyHole (Meta 2 Nothing Nothing)
      def = DefAST $ ASTDef (qualifyName "Main" "main") expr ty
   in Prog
        { progImports = mempty
        , progModule =
            Module
              { moduleName = "Main"
              , moduleTypes = mempty
              , moduleDefs = Map.singleton (baseName $ defName def) def
              }
        , progSelection = Nothing
        , progSmartHoles = SmartHoles
        , progLog = Log []
        }

-- | An initial app whose program is completely empty.
newEmptyApp :: App
newEmptyApp =
  App
    { appIdCounter = 3
    , appNameCounter = toEnum 0
    , appProg = newEmptyProg
    , appInit = NewEmptyApp
    }

-- | An initial program with some useful typedefs imported.
newProg :: Prog
newProg =
  newEmptyProg
    { progImports = [builtinModule, primitiveModule]
    , progModule =
        Module
          { moduleName = "Main"
          , moduleTypes = mempty
          , moduleDefs = defaultDefs "Main"
          }
    }

-- Since IDs should be unique in a module, we record 'defaultDefsNextID'
-- to initialise the 'appIdCounter'
defaultDefsNextId :: ID
defaultDefs :: ModuleName -> Map Name Def
(defaultDefs, defaultDefsNextId) =
  let (defs, nextID) = create $ do
        mainExpr <- emptyHole
        mainType <- tEmptyHole
        let astDefs m =
              [ ASTDef
                  { astDefName = qualifyName m "main"
                  , astDefExpr = mainExpr
                  , astDefType = mainType
                  }
              ]
        pure $ \m -> map DefAST $ astDefs m
   in (\m -> Map.fromList $ (\d -> (baseName $ defName d, d)) <$> defs m, nextID)

-- | An initial app whose program includes some useful definitions.
newApp :: App
newApp =
  newEmptyApp
    { appProg = newProg
    , appInit = NewApp
    , appIdCounter = fromEnum defaultDefsNextId
    }

-- | Construct a new, empty expression
newExpr :: MonadEditApp m => m Expr
newExpr = do
  id_ <- fresh
  pure $ EmptyHole (Meta id_ Nothing Nothing)

-- | Construct a new, empty type
newType :: MonadEditApp m => m Type
newType = do
  id_ <- fresh
  pure $ TEmptyHole (Meta id_ Nothing Nothing)

-- | Support for generating fresh IDs
instance MonadFresh ID EditAppM where
  fresh = do
    idCounter <- gets appIdCounter
    modify (\s -> s{appIdCounter = idCounter + 1})
    pure (ID idCounter)

-- | Support for generating names. Basically just a counter so we don't
-- generate the same automatic name twice.
instance MonadFresh NameCounter EditAppM where
  fresh = do
    nameCounter <- gets appNameCounter
    modify (\s -> s{appNameCounter = succ nameCounter})
    pure nameCounter

copyPasteSig :: MonadEditApp m => Prog -> (GVarName, ID) -> GVarName -> [Action] -> m Prog
copyPasteSig p (fromDefName, fromTyId) toDefName setup = do
  c' <- focusNodeImports p fromDefName fromTyId
  c <- case c' of
    Left (Left _) -> throwError $ CopyPasteError "tried to copy-paste an expression into a signature"
    Left (Right zt) -> pure $ Left zt
    Right zt -> pure $ Right zt
  smartHoles <- gets $ progSmartHoles . appProg
  -- We intentionally throw away any changes in doneSetup other than via 'tgt'
  -- as these could be in other definitions referencing this one, due to
  -- types changing. However, we are going to do a full tc pass anyway,
  -- which will pick up any problems. It is better to do it in one batch,
  -- in case the intermediate state after 'setup' causes more problems
  -- than the final state does.
  (oldDef, doneSetup) <- withDef (Just toDefName) p $ \def -> (def,) <$> applyActionsToTypeSig smartHoles (progImports p) (progModule p) def setup
  tgt <- case doneSetup of
    Left err -> throwError $ ActionError err
    Right (_, _, tgt) -> pure $ focusOnlyType tgt
  let sharedScope =
        if fromDefName == toDefName -- optimization only
          then getSharedScopeTy c $ Right tgt
          else mempty
  -- Delete unbound vars
  let cTgt = either target target c
      f (m, n) = if Set.member (unLocalName n) sharedScope then pure $ TVar m n else fresh <&> \i -> TEmptyHole (Meta i Nothing Nothing)
  cScoped <- traverseOf _freeVarsTy f cTgt
  freshCopy <- traverseOf (_typeMeta % _id) (const fresh) cScoped
  pasted <- case target tgt of
    TEmptyHole _ -> pure $ replace freshCopy tgt
    _ -> throwError $ CopyPasteError "copy/paste setup didn't select an empty hole"
  let newDef = oldDef{astDefType = fromZipper pasted}
  let newSel = NodeSelection SigNode (getID $ target pasted) (pasted ^. _target % _typeMetaLens % re _Right)
  let finalProg = addDef newDef p{progSelection = Just (Selection (astDefName newDef) $ Just newSel)}
  tcWholeProg finalProg

-- We cannot use bindersAbove as that works on names only, and different scopes
-- may reuse the same names. However, we want to detect that as non-shared.
-- Instead, we rely on fact that IDs are unique.
-- We get the scope from the second argument, as that is where we are pasting.
getSharedScopeTy :: Either TypeZ TypeZip -> Either TypeZ TypeZip -> Set.Set Name
getSharedScopeTy l r =
  let idsR = case r of
        Right r' -> getID r' : foldAbove ((: []) . getID . current) r'
        Left r' -> getID r' : foldAbove ((: []) . getID . current) (focusOnlyType r') <> (getID (unfocusType r') : foldAbove ((: []) . getID . current) r')
      -- Replae use of `unsafeHead` here. See:
      -- https://github.com/hackworthltd/primer/issues/147
      rID = unsafeHead idsR
      idsL = case l of
        Right l' -> getID l' : foldAbove ((: []) . getID . current) l'
        Left l' -> getID l' : foldAbove ((: []) . getID . current) (focusOnlyType l') <> (getID (unfocusType l') : foldAbove ((: []) . getID . current) l')
      commonAncestor = getLast $ foldMap (\(il, ir) -> Last $ if il == ir then Just il else Nothing) $ zip (reverse idsL) (reverse idsR)
      rAncestor = do
        a <- commonAncestor
        flip loopM r $ \r' -> if either getID getID r' == a then pure $ Right r' else Left <$> bimapM up up r'
      -- we need to pick up bindings that happen at the ancestor iff it
      -- is an actual ancestor (rather than l being a decendent of r)
      inScope =
        rAncestor <&> \case
          Left ra -> mwhen (rID /= getID ra) (Set.map unLocalName $ getBoundHereTy $ target ra) <> bindersAboveTypeZ ra
          Right ra -> Set.map unLocalName $ mwhen (rID /= getID ra) (getBoundHereTy $ target ra) <> bindersAboveTy ra
   in fromMaybe mempty inScope

-- TODO: there is a lot of duplicated code for copy/paste, often due to types/terms being different...
getSharedScope :: ExprZ -> ExprZ -> Set.Set Name
getSharedScope l r =
  let idsR = getID r : foldAbove ((: []) . getID . current) r
      idsL = getID l : foldAbove ((: []) . getID . current) l
      commonAncestor = getLast $ foldMap (\(il, ir) -> Last $ if il == ir then Just il else Nothing) $ zip (reverse idsL) (reverse idsR)
      rAncestorAndPenultimate = do
        a <- commonAncestor
        flip loopM (r, Nothing) $ \(r', p) -> if getID r' == a then pure $ Right (r', p) else Left . (,Just r') <$> up r'
      -- we need to pick up bindings that happen at the ancestor iff it
      -- is an actual ancestor (rather than l being a decendent of r)
      inScope =
        rAncestorAndPenultimate <&> \(ra, rp) ->
          let hereBound = maybe mempty (getBoundHere (target ra) . Just . target) rp -- we have a Just exactly when ra/=r
           in hereBound <> bindersAbove ra
   in fromMaybe mempty inScope

-- | A generalisation of 'when'
mwhen :: Monoid m => Bool -> m -> m
mwhen b m = if b then m else mempty

-- | Iterate until we get a 'Right'
loopM :: Monad m => (a -> m (Either a b)) -> a -> m b
loopM f a =
  f a >>= \case
    Left a' -> loopM f a'
    Right b -> pure b

-- | Checks every term definition in the editable module.
-- Does not check typedefs or imported modules.
tcWholeProg :: forall m. MonadEditApp m => Prog -> m Prog
tcWholeProg p =
  let tc :: ReaderT Cxt (ExceptT ActionError m) Prog
      tc = do
        defs' <- mapM checkDef (moduleDefs $ progModule p)
        let mod' = (progModule p){moduleDefs = defs'}
        let p' = p{progModule = mod'}
        -- We need to update the metadata cached in the selection
        let oldSel = progSelection p
        newSel <- case oldSel of
          Nothing -> pure Nothing
          Just s -> do
            let defName_ = s ^. #selectedDef
            updatedNode <- case s ^. #selectedNode of
              Nothing -> pure Nothing
              Just NodeSelection{nodeType, nodeId} -> do
                n <- runExceptT $ focusNode p' defName_ nodeId
                case (nodeType, n) of
                  (BodyNode, Right (Left x)) -> pure $ Just $ NodeSelection BodyNode nodeId $ bimap (view (position @1) . target) (view _typeMetaLens . target) x
                  (SigNode, Right (Right x)) -> pure $ Just $ NodeSelection SigNode nodeId $ x ^. _target % _typeMetaLens % re _Right
                  _ -> pure Nothing -- something's gone wrong: expected a SigNode, but found it in the body, or vv, or just not found it
            pure $
              Just $
                Selection
                  { selectedDef = defName_
                  , selectedNode = updatedNode
                  }
        pure $ p'{progSelection = newSel}
   in liftError ActionError $ runReaderT tc $ progCxt p

copyPasteBody :: MonadEditApp m => Prog -> (GVarName, ID) -> GVarName -> [Action] -> m Prog
copyPasteBody p (fromDefName, fromId) toDefName setup = do
  src' <- focusNodeImports p fromDefName fromId
  -- reassociate so get Expr+(Type+Type), rather than (Expr+Type)+Type
  let src = case src' of
        Left (Left e) -> Left e
        Left (Right t) -> Right (Left t)
        Right t -> Right (Right t)
  smartHoles <- gets $ progSmartHoles . appProg
  -- The Loc zipper captures all the changes, they are only reflected in the
  -- returned Def, which we thus ignore
  (oldDef, doneSetup) <- withDef (Just toDefName) p $ \def -> (def,) <$> applyActionsToBody smartHoles (progAllModules p) def setup
  tgt <- case doneSetup of
    Left err -> throwError $ ActionError err
    Right (_, tgt) -> pure tgt
  case (src, tgt) of
    (_, InBind _) -> throwError $ CopyPasteError "tried to paste an expression into a binder"
    (Left _, InType _) -> throwError $ CopyPasteError "tried to paste an expression into a type"
    (Right _, InExpr _) -> throwError $ CopyPasteError "tried to paste a type into an expression"
    (Right srcT, InType tgtT) -> do
      let sharedScope =
            if fromDefName == toDefName -- optimization only
              then getSharedScopeTy srcT $ Left tgtT
              else mempty
      -- Delete unbound vars. TODO: we may want to let-bind them?
      let srcSubtree = either target target srcT
          f (m, n) = if Set.member (unLocalName n) sharedScope then pure $ TVar m n else fresh <&> \i -> TEmptyHole (Meta i Nothing Nothing)
      scopedCopy <- traverseOf _freeVarsTy f srcSubtree
      freshCopy <- traverseOf (_typeMeta % _id) (const fresh) scopedCopy
      pasted <- case target tgtT of
        TEmptyHole _ -> pure $ replace freshCopy tgtT
        _ -> throwError $ CopyPasteError "copy/paste setup didn't select an empty hole"
      let newDef = oldDef{astDefExpr = unfocusExpr $ unfocusType pasted}
      let newSel = NodeSelection BodyNode (getID $ target pasted) (pasted ^. _target % _typeMetaLens % re _Right)
      let finalProg = addDef newDef p{progSelection = Just (Selection (astDefName newDef) $ Just newSel)}
      tcWholeProg finalProg
    (Left srcE, InExpr tgtE) -> do
      let sharedScope =
            if fromDefName == toDefName -- optimization only
              then getSharedScope srcE tgtE
              else mempty
      -- Delete unbound vars. TODO: we may want to let-bind them?
      let tm (m, n) = if Set.member (unLocalName n) sharedScope then pure $ Var m $ LocalVarRef n else fresh <&> \i -> EmptyHole (Meta i Nothing Nothing)
          ty (m, n) = if Set.member (unLocalName n) sharedScope then pure $ TVar m n else fresh <&> \i -> TEmptyHole (Meta i Nothing Nothing)
      scopedCopy <- traverseOf _freeTyVars ty =<< traverseOf _freeTmVars tm (target srcE)
      freshCopy <- traverseOf (_exprTypeMeta % _id) (const fresh) =<< traverseOf (_exprMeta % _id) (const fresh) scopedCopy
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
      let newSel = NodeSelection BodyNode (getID $ target pasted) (pasted ^. _target % _exprMetaLens % re _Left)
      let finalProg = addDef newDef p{progSelection = Just (Selection (astDefName newDef) $ Just newSel)}
      tcWholeProg finalProg

lookupASTDef :: GVarName -> Map GVarName Def -> Maybe ASTDef
lookupASTDef name = defAST <=< Map.lookup name

alterTypeDef ::
  MonadEditApp m =>
  (ASTTypeDef -> m ASTTypeDef) ->
  TyConName ->
  Module ->
  m Module
alterTypeDef f type_ m = do
  unless (qualifiedModule type_ == moduleName m) $ throwError $ TypeDefNotFound type_
  traverseOf
    #moduleTypes
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
  MonadEditApp m =>
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
liftError :: MonadEditApp m => (e -> ProgError) -> ExceptT e m b -> m b
liftError f = runExceptT >=> either (throwError . f) pure

allConNames :: Prog -> [ValConName]
allConNames =
  toListOf $
    #progModule
      % #moduleTypes
      % traversed
      % #_TypeDefAST
      % #astTypeDefConstructors
      % traversed
      % #valConName
