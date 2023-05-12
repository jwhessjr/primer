{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Compute all the possible actions which can be performed on a definition.
-- This module is expected to be imported qualified, due to various potential name clashes.
module Primer.Action.Available (
  Action (..),
  InputAction (..),
  NoInputAction (..),
  forDef,
  forBody,
  forSig,
  Option (..),
  FreeInput (..),
  Options (..),
  options,
) where

import Foreword

import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Tuple.Extra (fst3)
import Optics (
  to,
  (%),
  (^.),
  (^..),
  (^?),
  _Just,
 )
import Primer.App.Base (
  Editable (..),
  Level (..),
  NodeType (..),
 )
import Primer.Core (
  Expr,
  Expr' (..),
  GVarName,
  GlobalName (baseName, qualifiedModule),
  ID,
  ModuleName (unModuleName),
  Type,
  Type' (..),
  getID,
  unLocalName,
  _bindMeta,
  _chkedAt,
  _exprMetaLens,
  _synthed,
  _type,
  _typeMetaLens,
 )
import Primer.Core.Utils (freeVars)
import Primer.Def (
  ASTDef (..),
  DefMap,
 )
import Primer.Def.Utils (globalInUse)
import Primer.Name (unName)
import Primer.Questions (
  generateNameExpr,
  generateNameTy,
  variablesInScopeExpr,
  variablesInScopeTy,
 )
import Primer.TypeDef (
  ASTTypeDef (..),
  TypeDef (TypeDefAST),
  TypeDefMap,
  ValCon (valConArgs),
  typeDefAST,
  valConName,
 )
import Primer.Typecheck (
  Cxt,
  TypeDefError (TDIHoleType),
  TypeDefInfo (TypeDefInfo),
  getTypeDefInfo',
 )
import Primer.Zipper (
  SomeNode (..),
  findNodeWithParent,
  findType,
  focusOn,
  focusOnTy,
  locToEither,
 )

-- | An offered action.
data Action
  = NoInput NoInputAction
  | Input InputAction
  deriving stock (Eq, Ord, Show, Read)

-- | An action which can be applied without requiring further input.
data NoInputAction
  = MakeCase
  | MakeApp -- 6
  | MakeAPP -- 1
  | MakeAnn -- 1
  | RemoveAnn -- 3
  | LetToRec
  | Raise -- 35
  | EnterHole
  | RemoveHole
  | DeleteExpr -- 3
  | MakeFun -- 63
  | AddInput -- 2
  | MakeTApp -- 5
  | RaiseType
  | DeleteType -- 40
  | DuplicateDef
  | DeleteDef
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded)

-- | An action which requires extra data (often a name) before it can be applied.
data InputAction
  = MakeCon -- 1
  | MakeConSat
  | MakeVar -- 1
  | MakeVarSat
  | MakeLet
  | MakeLetRec
  | MakeLam -- 5
  | MakeLAM -- 2
  | RenamePattern
  | RenameLet
  | RenameLam
  | RenameLAM
  | MakeTCon
  | MakeTVar
  | MakeForall -- 6
  | RenameForall
  | RenameDef -- 35
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded)

forDef ::
  DefMap ->
  Editable ->
  GVarName ->
  [Action]
forDef _ NonEditable _ = mempty
forDef defs Editable defName =
    [Input RenameDef]

forBody ::
  TypeDefMap ->
  Level ->
  Editable ->
  Expr ->
  ID ->
  [Action]
forBody _ _ NonEditable _ _ = mempty
forBody tydefs l Editable expr id = case findNodeWithParent id expr of
  Nothing -> mempty
  Just (ExprNode e, p) ->
    let raiseAction = case p of
          Nothing -> [] -- at root already, cannot raise
          Just (ExprNode (Hole _ _)) -> [] -- in a NE hole, don't offer raise (as hole will probably just be recreated)
          _ -> [NoInput Raise]
     in forExpr tydefs l e <> raiseAction
  Just (TypeNode t, p) ->
    let raiseAction = case p of
          Just (ExprNode _) -> [] -- at the root of an annotation, so cannot raise
          _ -> [NoInput Raise]
     in forType l t <> raiseAction
  Just (CaseBindNode _, _) ->
    []

forSig ::
  Level ->
  Editable ->
  Type ->
  ID ->
  [Action]
forSig _ NonEditable _ _ = mempty
forSig l Editable ty id = case findType id ty of
  Nothing -> mempty
  Just t ->
    forType l t

forExpr :: TypeDefMap -> Level -> Expr -> [Action]
forExpr tydefs l expr =
  universalActions <> synOnly <> case expr of
    EmptyHole{} ->
      annotate
        <> [ Input MakeVar
           , Input MakeCon
           ]
    Hole{} ->
      delete
        <> annotate
    Ann{} ->
      delete
        <> mwhen (l == Expert) [NoInput RemoveAnn]
    Lam{} ->
      delete
        <> annotate
    LAM{} ->
      delete
        <> annotate
    Let _ v e _ ->
      delete
        <> annotate
        <> munless (unLocalName v `Set.member` freeVars e) []
    Letrec{} ->
      delete
        <> annotate
    _ ->
      delete
        <> annotate
  where
    universalActions = case l of
      Beginner ->
        [ Input MakeLam
        ]
      Intermediate ->
        [ Input MakeLam
        , NoInput MakeApp
        ]
      Expert ->
        [ NoInput MakeApp
        , NoInput MakeAPP
        , Input MakeLam
        , Input MakeLAM
        ]
    -- We assume that the input program is type-checked, in order to
    -- filter some actions by Syn/Chk
    synOnly =
      expr ^.. _exprMetaLens % _type % _Just % _synthed % to (getTypeDefInfo' tydefs) >>= \case
        Left TDIHoleType{} -> []
        Right (TypeDefInfo _ _ TypeDefAST{}) -> []
        _ -> []
    annotate = mwhen (l == Expert) [NoInput MakeAnn]
    delete = [NoInput DeleteExpr]

forType :: Level -> Type -> [Action]
forType l type_ =
  universalActions <> case type_ of
    TEmptyHole{} -> []
    TForall{} ->
      delete
    TFun{} ->
      delete <> [NoInput AddInput]
    _ ->
      delete
  where
    universalActions =
      [NoInput MakeFun]
        <> mwhen
          (l == Expert)
          [ Input MakeForall
          , NoInput MakeTApp
          ]
    delete = [NoInput DeleteType]

-- | An input for an 'InputAction'.
data Option = Option
  { option :: Text
  , context :: Maybe (NonEmpty Text)
  }
  deriving stock (Eq, Show, Read)

-- | The available sorts of free-form input for an 'InputAction'.
data FreeInput
  = -- | Free-form input is not allowed
    FreeNone
  | -- | A free-form string input is allowed, and represents a variable name
    FreeVarName
  | -- | A free-form string input is allowed, and represents a primitive integer
    FreeInt
  | -- | A free-form string input is allowed, and represents a primitive character
    FreeChar
  deriving stock (Show, Read, Bounded, Enum)

-- | The available inputs for an 'InputAction'.
data Options = Options
  { opts :: [Option]
  , free :: FreeInput
  }
  deriving stock (Show, Read)

options ::
  TypeDefMap ->
  DefMap ->
  Cxt ->
  Level ->
  ASTDef ->
  Maybe (NodeType, ID) ->
  InputAction ->
  -- | Returns 'Nothing' if an ID was required but not passed, passed but not found in the tree,
  -- or found but didn't correspond to the expected sort of entity (type/expr/pattern).
  Maybe Options
options typeDefs defs cxt level def mNodeSel = \case
  MakeCon ->
    pure
      . noFree
      . map (globalOpt . valConName . snd)
      . filter (not . (&& level == Beginner) . uncurry hasArgsCon)
      . concatMap (\td -> (td,) <$> astTypeDefConstructors td)
      . mapMaybe (typeDefAST . snd)
      $ Map.toList typeDefs
  MakeConSat ->
    pure
      . noFree
      . map (globalOpt . valConName . snd)
      . filter (uncurry hasArgsCon)
      . concatMap (\td -> (td,) <$> astTypeDefConstructors td)
      . mapMaybe (typeDefAST . snd)
      $ Map.toList typeDefs
  MakeVar ->
    varOpts
      <&> noFree . map fst . filter (not . (&& level == Beginner) . hasArgsVar . snd)
  MakeVarSat ->
    varOpts
      <&> noFree . map fst . filter (hasArgsVar . snd)
  MakeLet ->
    freeVar <$> genNames (Left Nothing)
  MakeLetRec ->
    freeVar <$> genNames (Left Nothing)
  MakeLam -> do
    ExprNode e <- findNode
    freeVar <$> genNames (Left $ join $ e ^? _exprMetaLens % _type % _Just % _chkedAt % to lamVarTy)
  MakeLAM -> do
    ExprNode e <- findNode
    freeVar <$> genNames (Right $ join $ e ^? _exprMetaLens % _type % _Just % _chkedAt % to lAMVarKind)
  RenamePattern -> do
    CaseBindNode b <- findNode
    freeVar <$> genNames (Left $ b ^? _bindMeta % _type % _Just % _chkedAt)
  RenameLet -> do
    ExprNode e <- findNode
    freeVar <$> genNames (Left $ e ^? _exprMetaLens % _type % _Just % _synthed)
  RenameLam -> do
    ExprNode e <- findNode
    freeVar <$> genNames (Left $ join $ e ^? _exprMetaLens % _type % _Just % _chkedAt % to lamVarTy)
  RenameLAM -> do
    ExprNode e <- findNode
    freeVar <$> genNames (Right $ join $ e ^? _exprMetaLens % _type % _Just % _chkedAt % to lAMVarKind)
  MakeTCon ->
    pure $ noFree $ globalOpt . fst <$> Map.toList typeDefs
  MakeTVar ->
    noFree . map (localOpt . unLocalName . fst) . fst3 <$> varsInScope
  MakeForall ->
    freeVar <$> genNames (Right Nothing)
  RenameForall -> do
    TypeNode t <- findNode
    freeVar <$> genNames (Right $ t ^. _typeMetaLens % _type)
  RenameDef ->
    pure $ freeVar []
  where
    freeVar opts = Options{opts, free = FreeVarName}
    noFree opts = Options{opts, free = FreeNone}
    localOpt = flip Option Nothing . unName
    globalOpt n =
      Option
        { option = unName $ baseName n
        , context = Just $ map unName $ unModuleName $ qualifiedModule n
        }
    varOpts = do
      (_, locals, globals) <- varsInScope
      pure $
        (first (localOpt . unLocalName) <$> locals)
          <> (first globalOpt <$> globals)
    findNode = do
      (nt, id) <- mNodeSel
      case nt of
        BodyNode -> fst <$> findNodeWithParent id (astDefExpr def)
        SigNode -> TypeNode <$> findType id (astDefType def)
    genNames typeOrKind = do
      z <- focusNode =<< mNodeSel
      pure $ map localOpt $ flip runReader cxt $ case z of
        Left zE -> generateNameExpr typeOrKind zE
        Right zT -> generateNameTy typeOrKind zT
    varsInScope = do
      nodeSel <- mNodeSel
      focusNode nodeSel <&> \case
        Left zE -> variablesInScopeExpr defs zE
        Right zT -> (variablesInScopeTy zT, [], [])
    focusNode (nt, id) = case nt of
      BodyNode -> Left . locToEither <$> focusOn id (astDefExpr def)
      SigNode -> fmap Right $ focusOnTy id $ astDefType def
    -- Extract the source of the function type we were checked at
    -- i.e. the type that a lambda-bound variable would have here
    lamVarTy = \case
      TFun _ s _ -> pure s
      _ -> Nothing
    -- Extract the kind a forall-bound variable would have here
    lAMVarKind = \case
      TForall _ _ k _ -> Just k
      _ -> Nothing
    -- Constructor has either type or value arguments
    hasArgsCon td vc =
      not (null (astTypeDefParameters td)) || not (null (valConArgs vc))
    -- Variable can be applied to something i.e. is a function or a polymorphic value
    hasArgsVar = \case
      TFun{} -> True
      TForall{} -> True
      _ -> False
