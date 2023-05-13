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
) where

import Foreword

import Data.Map qualified as Map
import Primer.App.Base (
  Editable (..),
 )
import Primer.Core (
  Expr,
  Expr' (..),
  GVarName,
  ID,
  Type,
  Type' (..),
 )
import Primer.Def (
  DefMap,
 )
import Primer.Def.Utils (globalInUse)
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
  Editable ->
  Expr ->
  ID ->
  [Action]
forBody NonEditable _ _ = mempty
forBody Editable expr id = case findNodeWithParent id expr of
  Nothing -> mempty
  Just (ExprNode _, p) -> case p of
          Nothing -> [] -- at root already, cannot raise
          Just (ExprNode (Hole _ _)) -> [] -- in a NE hole, don't offer raise (as hole will probably just be recreated)
          _ -> [NoInput Raise]
  Just (TypeNode t, p) ->
    let raiseAction = case p of
          Just (ExprNode _) -> [] -- at the root of an annotation, so cannot raise
          _ -> [NoInput Raise]
     in forType t <> raiseAction
  Just (CaseBindNode _, _) -> []

forSig ::
  Editable ->
  Type ->
  ID ->
  [Action]
forSig NonEditable _ _ = mempty
forSig Editable ty id = case findType id ty of
  Nothing -> mempty
  Just t -> forType t

forType :: Type -> [Action]
forType type_ =
  [NoInput MakeFun] <> case type_ of
    TEmptyHole{} -> []
    _ -> [NoInput DeleteType]

-- | An input for an 'InputAction'.
data Option = Option
  { option :: Text
  }
  deriving stock (Eq, Show, Read)
