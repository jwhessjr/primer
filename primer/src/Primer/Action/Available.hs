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
import Optics (
  to,
  (%),
  (^..),
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
  ID,
  Type,
  Type' (..),
  unLocalName,
  _exprMetaLens,
  _synthed,
  _type,
 )
import Primer.Core.Utils (freeVars)
import Primer.Def (
  ASTDef (..),
  DefMap,
 )
import Primer.Def.Utils (globalInUse)
import Primer.TypeDef (
  TypeDef (TypeDefAST),
  TypeDefMap,
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
 )

-- | An offered action.
data Action
  = NoInput NoInputAction
  | Input InputAction
  deriving stock (Eq, Ord, Show, Read)

-- | An action which can be applied without requiring further input.
data NoInputAction
  = Raise -- 35
  | MakeFun -- 63
  | DeleteType -- 40
  | DeleteDef
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded)

-- | An action which requires extra data (often a name) before it can be applied.
data InputAction
  = RenameDef -- 35
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded)

forDef ::
  DefMap ->
  Editable ->
  GVarName ->
  [Action]
forDef _ NonEditable _ = mempty
forDef defs Editable defName =
    [Input RenameDef]
      <> mwhen
        -- ensure the definition is not in use, otherwise the action will not succeed
        (not $ globalInUse defName $ Map.delete defName defs)
        [NoInput DeleteDef]

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
    Hole{} ->
      delete
        <> annotate
    Ann{} ->
      delete
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
        [
        ]
      Intermediate ->
        [
        ]
      Expert ->
        [
        ]
    -- We assume that the input program is type-checked, in order to
    -- filter some actions by Syn/Chk
    synOnly =
      expr ^.. _exprMetaLens % _type % _Just % _synthed % to (getTypeDefInfo' tydefs) >>= \case
        Left TDIHoleType{} -> []
        Right (TypeDefInfo _ _ TypeDefAST{}) -> []
        _ -> []
    annotate = mwhen (l == Expert) []
    delete = []

forType :: Level -> Type -> [Action]
forType l type_ =
  universalActions <> case type_ of
    TEmptyHole{} -> []
    TForall{} ->
      delete
    TFun{} ->
      delete
    _ ->
      delete
  where
    universalActions =
      [NoInput MakeFun]
        <> mwhen
          (l == Expert)
          [
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
  RenameDef ->
    pure $ freeVar []
  where
    freeVar opts = Options{opts, free = FreeVarName}
