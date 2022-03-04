{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}

module Tests.Action.Prog where

import Foreword

import Control.Monad.Fresh
import qualified Data.Map.Strict as Map
import Data.Tuple.Extra (snd3)
import Optics
import Primer.Action (
  Action (
    ConstructAnn,
    ConstructArrowL,
    ConstructGlobalVar,
    ConstructLet,
    ConstructTCon,
    Delete,
    EnterType,
    Move
  ),
  Movement (Branch, Child1, Child2),
 )
import Primer.App (
  App (..),
  Log (..),
  NodeSelection (..),
  NodeType (..),
  Prog (..),
  ProgAction (..),
  ProgError (..),
  Question (VariablesInScope),
  Selection (..),
  handleEditRequest,
  handleQuestion,
  lookupASTDef,
  newApp,
  newEmptyApp,
  newEmptyProg,
  newProg,
  tcWholeProg,
 )
import Primer.Core (
  ASTDef (..),
  ASTTypeDef (..),
  Def (..),
  Expr' (..),
  ID,
  Kind (KType),
  Meta (..),
  Type' (..),
  TypeDef (..),
  ValCon (..),
  defID,
  defName,
  getID,
  _exprMeta,
  _exprTypeMeta,
  _id,
  _typeMeta,
 )
import Primer.Core.DSL (
  S,
  aPP,
  ann,
  app,
  branch,
  case_,
  con,
  create,
  emptyHole,
  lAM,
  lam,
  tEmptyHole,
  tapp,
  tcon,
  tforall,
  tfun,
  tvar,
  var,
 )
import Primer.Module (Module (Module, moduleDefs, moduleTypes))
import Primer.Name
import Primer.Primitives (allPrimTypeDefs)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, (@=?), (@?=))
import TestM (TestM, evalTestM)
import TestUtils (withPrimDefs)

unit_empty_actions_only_change_the_log :: Assertion
unit_empty_actions_only_change_the_log = progActionTest defaultEmptyProg [] $
  expectSuccess $ \prog prog' ->
    prog' @?= prog{progLog = Log [[]]}

-- We can move to the default def in a program
-- (this may only exist at the start of a session)
unit_move_to_def_main :: Assertion
unit_move_to_def_main = progActionTest defaultEmptyProg [MoveToDef 0] $
  expectSuccess $ \prog prog' ->
    prog' @?= prog{progLog = Log [[MoveToDef 0]]}

-- Expression actions are tested in ActionTest - here we just check that we can modify the correct
-- def.
unit_move_to_def_and_construct_let :: Assertion
unit_move_to_def_and_construct_let =
  progActionTest defaultEmptyProg [MoveToDef 2, BodyAction [ConstructLet (Just "x")]] $
    expectSuccess $ \prog prog' ->
      case astDefExpr <$> lookupASTDef 2 (moduleDefs $ progModule prog') of
        Just Let{} ->
          -- Check that main is unchanged
          Map.lookup 0 (moduleDefs $ progModule prog') @?= Map.lookup 0 (moduleDefs $ progModule prog)
        _ -> assertFailure "definition not found"

unit_rename_def :: Assertion
unit_rename_def =
  progActionTest defaultEmptyProg [RenameDef 2 "foo"] $
    expectSuccess $ \_ prog' -> do
      fmap defName (Map.lookup 2 (moduleDefs $ progModule prog')) @?= Just "foo"
      fmap defName (Map.lookup 0 (moduleDefs $ progModule prog')) @?= Just "main"

unit_rename_def_to_same_name_as_existing_def :: Assertion
unit_rename_def_to_same_name_as_existing_def =
  progActionTest defaultEmptyProg [RenameDef 2 "main"] $
    expectError (@?= DefAlreadyExists "main" 0)

unit_rename_def_to_same_name_as_existing_def_prim :: Assertion
unit_rename_def_to_same_name_as_existing_def_prim =
  progActionTest defaultPrimsProg [RenameDef 2 "toUpper"] $
    expectError (@?= DefAlreadyExists "toUpper" 118)

unit_delete_def :: Assertion
unit_delete_def =
  progActionTest defaultEmptyProg [DeleteDef 2] $
    expectSuccess $ \_ prog' -> do
      fmap defName (Map.lookup 2 (moduleDefs $ progModule prog')) @?= Nothing
      fmap defName (Map.lookup 0 (moduleDefs $ progModule prog')) @?= Just "main"

unit_delete_def_unknown_id :: Assertion
unit_delete_def_unknown_id =
  progActionTest defaultEmptyProg [DeleteDef 99] $
    expectError (@?= DefNotFound 99)

unit_delete_def_used_id :: Assertion
unit_delete_def_used_id =
  progActionTest defaultEmptyProg [MoveToDef 0, BodyAction [ConstructGlobalVar 2], DeleteDef 2] $
    expectError (@?= DefInUse 2)

-- 'foo = foo' shouldn't count as "in use" and block deleting itself
unit_delete_def_recursive :: Assertion
unit_delete_def_recursive =
  progActionTest defaultEmptyProg [MoveToDef 0, BodyAction [ConstructGlobalVar 0], DeleteDef 0] $
    expectSuccess $ \prog prog' -> Map.delete 0 (moduleDefs $ progModule prog) @?= moduleDefs (progModule prog')

unit_move_to_unknown_def :: Assertion
unit_move_to_unknown_def =
  progActionTest defaultEmptyProg [MoveToDef 5] $ expectError (@?= DefNotFound 5)

unit_rename_unknown_def :: Assertion
unit_rename_unknown_def =
  progActionTest defaultEmptyProg [RenameDef 5 "foo"] $ expectError (@?= DefNotFound 5)

unit_construct_let_without_moving_to_def_first :: Assertion
unit_construct_let_without_moving_to_def_first =
  progActionTest defaultEmptyProg [BodyAction [ConstructLet (Just "x")]] $ expectError (@?= NoDefSelected)

unit_create_def :: Assertion
unit_create_def = progActionTest defaultEmptyProg [CreateDef $ Just "newDef"] $
  expectSuccess $ \_ prog' -> do
    case lookupASTDef 4 (moduleDefs $ progModule prog') of
      Nothing -> assertFailure $ show $ moduleDefs $ progModule prog'
      Just def -> do
        astDefID def @?= 4
        astDefName def @?= "newDef"
        astDefExpr def @?= EmptyHole (Meta 5 Nothing Nothing)

unit_create_def_clash_prim :: Assertion
unit_create_def_clash_prim =
  progActionTest defaultPrimsProg [CreateDef $ Just "toUpper"] $
    expectError (@?= DefAlreadyExists "toUpper" 118)

unit_create_typedef :: Assertion
unit_create_typedef =
  let lst =
        ASTTypeDef
          { astTypeDefName = "List"
          , astTypeDefParameters = [("a", KType)]
          , astTypeDefConstructors =
              [ ValCon "Nil" []
              , ValCon "Cons" [TVar () "a", TApp () (TCon () "List") (TVar () "a")]
              ]
          , astTypeDefNameHints = ["xs", "ys", "zs"]
          }
      tree =
        ASTTypeDef
          { astTypeDefName = "Tree"
          , astTypeDefParameters = [("a", KType)]
          , astTypeDefConstructors = [ValCon "Node" [TVar () "a", TApp () (TCon () "List") (TApp () (TCon () "Tree") (TVar () "a"))]]
          , astTypeDefNameHints = ["xs", "ys", "zs"]
          }
   in progActionTest defaultEmptyProg [AddTypeDef lst, AddTypeDef tree] $
        expectSuccess $
          \_ prog' -> do
            case moduleTypes $ progModule prog' of
              [lst', tree'] -> do
                TypeDefAST lst @=? lst'
                TypeDefAST tree @=? tree'
              _ -> assertFailure $ show $ moduleTypes $ progModule prog'

-- "List" is unknown here
unit_create_typedef_bad_1 :: Assertion
unit_create_typedef_bad_1 =
  let td =
        ASTTypeDef
          { astTypeDefName = "Tree"
          , astTypeDefParameters = [("a", KType)]
          , astTypeDefConstructors = [ValCon "Node" [TVar () "a", TApp () (TCon () "List") (TApp () (TCon () "Tree") (TVar () "a"))]]
          , astTypeDefNameHints = ["xs", "ys", "zs"]
          }
   in progActionTest defaultEmptyProg [AddTypeDef td] $
        expectError (@?= TypeDefError "UnknownTypeConstructor \"List\"")

-- duplicate type(names) added
unit_create_typedef_bad_2 :: Assertion
unit_create_typedef_bad_2 =
  let td1 =
        ASTTypeDef
          { astTypeDefName = "T"
          , astTypeDefParameters = []
          , astTypeDefConstructors = []
          , astTypeDefNameHints = []
          }
      td2 =
        ASTTypeDef
          { astTypeDefName = "T"
          , astTypeDefParameters = []
          , astTypeDefConstructors = []
          , astTypeDefNameHints = []
          }
   in progActionTest defaultEmptyProg [AddTypeDef td1, AddTypeDef td2] $
        expectError (@?= TypeDefError "InternalError \"Duplicate-ly-named TypeDefs\"")

-- Forbid duplicate constructor names within one type
unit_create_typedef_bad_3 :: Assertion
unit_create_typedef_bad_3 =
  let td =
        ASTTypeDef
          { astTypeDefName = "T"
          , astTypeDefParameters = []
          , astTypeDefConstructors =
              [ ValCon "C" []
              , ValCon "C" []
              ]
          , astTypeDefNameHints = []
          }
   in progActionTest defaultEmptyProg [AddTypeDef td] $
        expectError (@?= TypeDefError "InternalError \"Duplicate-ly-named constructor (perhaps in different typedefs)\"")

-- Forbid duplicate constructor names across types
unit_create_typedef_bad_4 :: Assertion
unit_create_typedef_bad_4 =
  let td1 =
        ASTTypeDef
          { astTypeDefName = "T1"
          , astTypeDefParameters = []
          , astTypeDefConstructors = [ValCon "C" []]
          , astTypeDefNameHints = []
          }
      td2 =
        ASTTypeDef
          { astTypeDefName = "T2"
          , astTypeDefParameters = []
          , astTypeDefConstructors = [ValCon "C" []]
          , astTypeDefNameHints = []
          }
   in progActionTest defaultEmptyProg [AddTypeDef td1, AddTypeDef td2] $
        expectError (@?= TypeDefError "InternalError \"Duplicate-ly-named constructor (perhaps in different typedefs)\"")

-- Forbid duplicate parameter names
unit_create_typedef_bad_5 :: Assertion
unit_create_typedef_bad_5 =
  let td =
        ASTTypeDef
          { astTypeDefName = "T"
          , astTypeDefParameters = [("a", KType), ("a", KType)]
          , astTypeDefConstructors = []
          , astTypeDefNameHints = []
          }
   in progActionTest defaultEmptyProg [AddTypeDef td] $
        expectError (@?= TypeDefError "InternalError \"Duplicate names in one tydef: between parameter-names and constructor-names\"")

-- Forbid clash between type name and parameter name
unit_create_typedef_bad_6 :: Assertion
unit_create_typedef_bad_6 =
  let td =
        ASTTypeDef
          { astTypeDefName = "T"
          , astTypeDefParameters = [("T", KType)]
          , astTypeDefConstructors = []
          , astTypeDefNameHints = []
          }
   in progActionTest defaultEmptyProg [AddTypeDef td] $
        expectError (@?= TypeDefError "InternalError \"Duplicate names in one tydef: between type-def-name and parameter-names\"")

-- Forbid clash between parameter name and constructor name
unit_create_typedef_bad_7 :: Assertion
unit_create_typedef_bad_7 =
  let td =
        ASTTypeDef
          { astTypeDefName = "T"
          , astTypeDefParameters = [("a", KType)]
          , astTypeDefConstructors = [ValCon "a" []]
          , astTypeDefNameHints = []
          }
   in progActionTest defaultEmptyProg [AddTypeDef td] $
        expectError (@?= TypeDefError "InternalError \"Duplicate names in one tydef: between parameter-names and constructor-names\"")

-- Forbid clash between type name and name of a primitive type
unit_create_typedef_bad_prim :: Assertion
unit_create_typedef_bad_prim =
  let td =
        ASTTypeDef
          { astTypeDefName = "Char"
          , astTypeDefParameters = []
          , astTypeDefConstructors = []
          , astTypeDefNameHints = []
          }
   in progActionTest defaultPrimsProg [AddTypeDef td] $
        expectError (@?= TypeDefError "InternalError \"Duplicate-ly-named TypeDefs\"")

-- Allow clash between type name and constructor name in one type
unit_create_typedef_8 :: Assertion
unit_create_typedef_8 =
  let td =
        ASTTypeDef
          { astTypeDefName = "T"
          , astTypeDefParameters = []
          , astTypeDefConstructors = [ValCon "T" []]
          , astTypeDefNameHints = []
          }
   in progActionTest defaultEmptyProg [AddTypeDef td] $
        expectSuccess $ \_ prog' -> moduleTypes (progModule prog') @?= [TypeDefAST td]

-- Allow clash between type name and constructor name across types
unit_create_typedef_9 :: Assertion
unit_create_typedef_9 =
  let td1 =
        ASTTypeDef
          { astTypeDefName = "T"
          , astTypeDefParameters = []
          , astTypeDefConstructors = [ValCon "C" []]
          , astTypeDefNameHints = []
          }
      td2 =
        ASTTypeDef
          { astTypeDefName = "C"
          , astTypeDefParameters = []
          , astTypeDefConstructors = []
          , astTypeDefNameHints = []
          }
   in progActionTest defaultEmptyProg [AddTypeDef td1, AddTypeDef td2] $
        expectSuccess $ \_ prog' -> moduleTypes (progModule prog') @?= [TypeDefAST td1, TypeDefAST td2]

unit_construct_arrow_in_sig :: Assertion
unit_construct_arrow_in_sig =
  progActionTest defaultEmptyProg [MoveToDef 2, SigAction [ConstructArrowL, Move Child1]] $
    expectSuccess $ \_ prog' ->
      case lookupASTDef 2 (moduleDefs $ progModule prog') of
        Just def ->
          -- Check that the signature is an arrow type
          case astDefType def of
            TFun _ lhs _ ->
              -- Check that the selection is focused on the lhs, as we instructed
              case progSelection prog' of
                Just (Selection d (Just NodeSelection{nodeType = SigNode, nodeId})) -> do
                  astDefID d @?= astDefID def
                  nodeId @?= getID lhs
                _ -> assertFailure "no selection"
            _ -> assertFailure "not a function"
        _ -> assertFailure "definition not found"

unit_sigaction_creates_holes :: Assertion
unit_sigaction_creates_holes =
  let acts =
        [ -- main :: Char
          MoveToDef 0
        , SigAction [ConstructTCon "Char"]
        , -- other :: Char; other = main
          MoveToDef 2
        , SigAction [ConstructTCon "Char"]
        , BodyAction [ConstructGlobalVar 0]
        , -- main :: Int
          -- We expect this to change 'other' to contain a hole
          MoveToDef 0
        , SigAction [Delete, ConstructTCon "Int"]
        ]
   in progActionTest defaultPrimsProg acts $
        expectSuccess $ \_ prog' ->
          case lookupASTDef 2 (moduleDefs $ progModule prog') of
            Just def ->
              -- Check that the definition is a non-empty hole
              case astDefExpr def of
                Hole _ (GlobalVar _ 0) -> pure ()
                _ -> assertFailure "expected {? main ?}"
            _ -> assertFailure "definition not found"

unit_copy_paste_duplicate :: Assertion
unit_copy_paste_duplicate = do
  let ((p, fromDef, fromType, fromExpr, toDef, _toType, _toExpr), maxID) = create $ do
        mainType <- tforall "a" KType (tvar "a" `tfun` (tcon "Maybe" `tapp` tEmptyHole))
        mainExpr <- lAM "b" $ lam "x" $ con "Just" `aPP` tvar "b" `app` var "x"
        mainID <- fresh
        let mainDef = ASTDef mainID "main" mainExpr mainType
        blankID <- fresh
        blankDef <- ASTDef blankID "blank" <$> emptyHole <*> tEmptyHole
        pure
          ( newProg{progSelection = Nothing}
            & #progModule % #moduleDefs .~ Map.fromList [(mainID, DefAST mainDef), (blankID, DefAST blankDef)]
          , mainID
          , getID mainType
          , getID mainExpr
          , blankID
          , getID (astDefType blankDef)
          , getID (astDefExpr blankDef)
          )
  let a = newApp{appProg = p}
      actions = [MoveToDef toDef, CopyPasteSig (fromDef, fromType) [], CopyPasteBody (fromDef, fromExpr) []]
      (result, _) = runAppTestM maxID a $ (,) <$> tcWholeProg p <*> handleEditRequest actions
  case result of
    Left e -> assertFailure $ show e
    Right (tcp, r) ->
      -- use the typechecked input p, as the result will have had a typecheck run, so
      -- we need the cached kinds to match up
      let src = lookupASTDef fromDef (moduleDefs $ progModule tcp)
          clearIDs = set (_Just % _defIDs) 0
       in do
            src @?= lookupASTDef fromDef (moduleDefs $ progModule r)
            assertBool "equal to toDef" $ src /= lookupASTDef toDef (moduleDefs $ progModule r)
            clearIDs (set (_Just % #astDefName) "blank" src) @?= clearIDs (lookupASTDef toDef (moduleDefs $ progModule r))

-- ∀a . (∀b,c . a -> b -> ∀d. c -> d)  -> ∀c. ?
-- copy         ^------------------^
-- paste                                      ^
-- should result in
-- ∀a . (∀b,c . a -> b -> ∀d. c -> d)  -> ∀c. a -> ? -> ∀d. ? -> d
--
-- This tests that we handle scoping correctly:
-- - The a is in-scope: so copied
-- - The b is out-of-scope, so replaced with a hole
-- - The c is out-of-scope (even though the target has a 'c' in-scope, it is a
--   different binder), so replace with a hole
-- - The d is bound within the copied subtree, so it is in-scope
unit_copy_paste_type_scoping :: Assertion
unit_copy_paste_type_scoping = do
  let ((pInitial, definitionID, srcID, pExpected), maxID) = create $ do
        toCopy <- tvar "a" `tfun` tvar "b" `tfun` tforall "d" KType (tvar "c" `tfun` tvar "d")
        let skel r = tforall "a" KType $ tfun (tforall "b" KType $ tforall "c" KType $ pure toCopy) $ tforall "c" KType r
        definitionID' <- fresh
        defInitial <- ASTDef definitionID' "main" <$> emptyHole <*> skel tEmptyHole
        expected <- ASTDef definitionID' "main" <$> emptyHole <*> skel (tvar "a" `tfun` tEmptyHole `tfun` tforall "d" KType (tEmptyHole `tfun` tvar "d"))
        pure
          ( newEmptyProg & #progModule % #moduleDefs .~ Map.fromList [(definitionID', DefAST defInitial)]
          , definitionID'
          , getID toCopy
          , newEmptyProg & #progModule % #moduleDefs .~ Map.fromList [(definitionID', DefAST expected)]
          )
  let a = newEmptyApp{appProg = pInitial}
      actions = [MoveToDef definitionID, CopyPasteSig (definitionID, srcID) [Move Child1, Move Child2, Move Child1]]
      (result, _) = runAppTestM maxID a $ (,) <$> tcWholeProg pExpected <*> handleEditRequest actions
  case result of
    Left e -> assertFailure $ show e
    Right (tcpExpected, r) ->
      -- use the typechecked input p, as the result will have had a typecheck run, so
      -- we need the cached kinds to match up
      let clearIDs = set (traversed % #_DefAST % _defIDs) 0
       in -- clearIDs (set (_Just % #defName) "blank" src ) @?= clearIDs (Map.lookup toDef (progDefs r))
          clearIDs (moduleDefs $ progModule r) @?= clearIDs (moduleDefs $ progModule tcpExpected)

-- ∀a b.a ~> ∀a.a
unit_raise :: Assertion
unit_raise = do
  let ((pInitial, definitionID, srcID, pExpected), maxID) = create $ do
        definitionID' <- fresh
        toCopy <- tvar "a"
        defInitial <- ASTDef definitionID' "main" <$> emptyHole <*> tforall "a" KType (tforall "b" KType $ pure toCopy)
        expected <- ASTDef definitionID' "main" <$> emptyHole <*> tforall "a" KType (tvar "a")
        pure
          ( newEmptyProg & #progModule % #moduleDefs .~ Map.fromList [(definitionID', DefAST defInitial)]
          , definitionID'
          , getID toCopy
          , newEmptyProg & #progModule % #moduleDefs .~ Map.fromList [(definitionID', DefAST expected)]
          )
  let a = newEmptyApp{appProg = pInitial}
      actions = [MoveToDef definitionID, CopyPasteSig (definitionID, srcID) [Move Child1, Delete]]
      (result, _) = runAppTestM maxID a $ (,) <$> tcWholeProg pExpected <*> handleEditRequest actions
  case result of
    Left e -> assertFailure $ show e
    Right (tcpExpected, r) ->
      -- use the typechecked input p, as the result will have had a typecheck run, so
      -- we need the cached kinds to match up
      let clearIDs = set (traversed % #_DefAST % _defIDs) 0
       in clearIDs (moduleDefs $ progModule r) @?= clearIDs (moduleDefs $ progModule tcpExpected)

-- ∀a. List a -> ∀b. b -> Pair a b
-- /\a . λ x . case x of Nil -> ? ; Cons y ys -> /\@b z -> Pair @a @b y z
-- copy the Just @a y into the hole to get
-- /\a . λ x . case x of Nil -> lettype b = ? in let y = ? : a in Pair @a @b y z ; Cons y ys -> /\@b z -> Pair @a @b y z
unit_copy_paste_expr_1 :: Assertion
unit_copy_paste_expr_1 = do
  let ((pInitial, definitionID, srcID, pExpected), maxID) = create $ do
        definitionID' <- fresh
        ty <- tforall "a" KType $ (tcon "List" `tapp` tvar "a") `tfun` tforall "b" KType (tvar "b" `tfun` (tcon "Pair" `tapp` tvar "a" `tapp` tvar "b"))
        let toCopy' = con "MakePair" `aPP` tvar "a" `aPP` tvar "b" `app` var "y" `app` var "z" -- want different IDs for the two occurences in expected
        toCopy <- toCopy'
        let skel r =
              lAM "a" $
                lam "x" $
                  case_
                    (var "x")
                    [ branch "Nil" [] r
                    , branch "Cons" [("y", Nothing), ("ys", Nothing)] $ lAM "b" $ lam "z" $ pure toCopy
                    ]
        expectPasted <- con "MakePair" `aPP` tvar "a" `aPP` tEmptyHole `app` emptyHole `app` emptyHole
        -- TODO: in the future we may want to insert let bindings for variables
        -- which are out of scope in the target, and produce something like
        -- expectPasted <- letType "b" tEmptyHole $ let_ "y" (emptyHole `ann` tvar "a") $ let_ "z" (emptyHole `ann` tvar "b") toCopy'
        defInitial <- ASTDef definitionID' "main" <$> skel emptyHole <*> pure ty
        expected <- ASTDef definitionID' "main" <$> skel (pure expectPasted) <*> pure ty
        pure
          ( newProg & #progModule % #moduleDefs .~ Map.fromList [(definitionID', DefAST defInitial)]
          , definitionID'
          , getID toCopy
          , newProg & #progModule % #moduleDefs .~ Map.fromList [(definitionID', DefAST expected)]
          )
  let a = newApp{appProg = pInitial}
      actions = [MoveToDef definitionID, CopyPasteBody (definitionID, srcID) [Move Child1, Move Child1, Move (Branch "Nil")]]
      (result, _) = runAppTestM maxID a $ (,) <$> tcWholeProg pExpected <*> handleEditRequest actions
  case result of
    Left e -> assertFailure $ show e
    Right (tcpExpected, r) ->
      -- use the typechecked input p, as the result will have had a typecheck run, so
      -- we need the cached kinds to match up
      let clearIDs = set (traversed % #_DefAST % _defIDs) 0
       in clearIDs (moduleDefs $ progModule r) @?= clearIDs (moduleDefs $ progModule tcpExpected)

unit_copy_paste_ann :: Assertion
unit_copy_paste_ann = do
  let ((p, fromDef, fromAnn, toDef), maxID) = create $ do
        toCopy <- tcon "Bool"
        mainID <- fresh
        mainDef <- ASTDef mainID "main" <$> emptyHole `ann` pure toCopy <*> tEmptyHole
        blankID <- fresh
        blankDef <- ASTDef blankID "blank" <$> emptyHole `ann` tEmptyHole <*> tEmptyHole
        pure
          ( newProg{progSelection = Nothing} & #progModule % #moduleDefs .~ Map.fromList [(mainID, DefAST mainDef), (blankID, DefAST blankDef)]
          , mainID
          , getID toCopy
          , blankID
          )
  let a = newApp{appProg = p}
      actions = [MoveToDef toDef, CopyPasteBody (fromDef, fromAnn) [EnterType]]
  let (result, _) = runAppTestM maxID a $ (,) <$> tcWholeProg p <*> handleEditRequest actions
  case result of
    Left e -> assertFailure $ show e
    Right (tcp, r) ->
      -- use the typechecked input p, as the result will have had a typecheck run, so
      -- we need the cached kinds to match up
      let src = lookupASTDef fromDef (moduleDefs $ progModule tcp)
          clearIDs = set (_Just % _defIDs) 0
       in do
            src @?= lookupASTDef fromDef (moduleDefs $ progModule r)
            assertBool "equal to toDef" $ src /= lookupASTDef toDef (moduleDefs $ progModule r)
            clearIDs (set (_Just % #astDefName) "blank" src) @?= clearIDs (lookupASTDef toDef (moduleDefs $ progModule r))

unit_copy_paste_ann2sig :: Assertion
unit_copy_paste_ann2sig = do
  let ((pInitial, definitionID, srcID, pExpected), maxID) = create $ do
        definitionID' <- fresh
        toCopy <- tcon "Bool"
        defInitial <- ASTDef definitionID' "main" <$> emptyHole `ann` pure toCopy <*> tEmptyHole
        expected <- ASTDef definitionID' "main" <$> emptyHole `ann` pure toCopy <*> tcon "Bool"
        pure
          ( newProg & #progModule % #moduleDefs .~ Map.fromList [(definitionID', DefAST defInitial)]
          , definitionID'
          , getID toCopy
          , newProg & #progModule % #moduleDefs .~ Map.fromList [(definitionID', DefAST expected)]
          )
  let a = newApp{appProg = pInitial}
      actions = [MoveToDef definitionID, CopyPasteSig (definitionID, srcID) []]
      (result, _) = runAppTestM maxID a $ (,) <$> tcWholeProg pExpected <*> handleEditRequest actions
  case result of
    Left e -> assertFailure $ show e
    Right (tcpExpected, r) ->
      -- use the typechecked input p, as the result will have had a typecheck run, so
      -- we need the cached kinds to match up
      let clearIDs = set (traversed % #_DefAST % _defIDs) 0
       in clearIDs (moduleDefs $ progModule r) @?= clearIDs (moduleDefs $ progModule tcpExpected)

unit_copy_paste_sig2ann :: Assertion
unit_copy_paste_sig2ann = do
  let ((pInitial, definitionID, srcID, pExpected), maxID) = create $ do
        definitionID' <- fresh
        toCopy <- tcon "Bool"
        defInitial <- ASTDef definitionID' "main" <$> emptyHole <*> pure toCopy
        expected <- ASTDef definitionID' "main" <$> emptyHole `ann` tcon "Bool" <*> pure toCopy
        pure
          ( newProg & #progModule % #moduleDefs .~ Map.fromList [(definitionID', DefAST defInitial)]
          , definitionID'
          , getID toCopy
          , newProg & #progModule % #moduleDefs .~ Map.fromList [(definitionID', DefAST expected)]
          )
  let a = newApp{appProg = pInitial}
      actions = [MoveToDef definitionID, CopyPasteBody (definitionID, srcID) [ConstructAnn, EnterType]]
      (result, _) = runAppTestM maxID a $ (,) <$> tcWholeProg pExpected <*> handleEditRequest actions
  case result of
    Left e -> assertFailure $ show e
    Right (tcpExpected, r) ->
      -- use the typechecked input p, as the result will have had a typecheck run, so
      -- we need the cached kinds to match up
      let clearIDs = set (traversed % #_DefAST % _defIDs) 0
       in clearIDs (moduleDefs $ progModule r) @?= clearIDs (moduleDefs $ progModule tcpExpected)

-- VariablesInScope sees imported terms
unit_import_vars :: Assertion
unit_import_vars =
  let (prog, maxID) = create defaultImportProg
      a = newEmptyApp{appProg = prog}
      test = do
        prog' <- handleEditRequest [CreateDef Nothing]
        case Map.assocs $ moduleDefs $ progModule prog' of
          [(i, DefAST d)] -> do
            a' <- get
            (_, vs) <- runReaderT (handleQuestion (VariablesInScope i $ getID $ astDefExpr d)) a'
            pure $
              assertBool "VariablesInScope did not report the imported Int.+" $
                any ((== "Int.+") . snd3) vs
          _ -> pure $ assertFailure "Expected one def which was just created"
   in case fst $ runAppTestM maxID a test of
        Left err -> assertFailure $ show err
        Right assertion -> assertion

-- Can reference something in an imported module (both types and terms)
unit_import_reference :: Assertion
unit_import_reference =
  let (prog, maxID) = create defaultImportProg
      a = newEmptyApp{appProg = prog}
      test = do
        prog' <- handleEditRequest [CreateDef Nothing]
        case (findGlobalByName prog' "toUpper", Map.assocs $ moduleDefs $ progModule prog') of
          (Just toUpperDef, [(i, _)]) -> do
            _ <-
              handleEditRequest
                [ MoveToDef i
                , SigAction [ConstructTCon "Char"]
                , BodyAction [ConstructGlobalVar $ defID toUpperDef]
                ]
            pure $ pure ()
          (Nothing, _) -> pure $ assertFailure "Could not find the imported toUpper"
          (Just _, _) -> pure $ assertFailure "Expected one def which was just created"
   in case fst $ runAppTestM maxID a test of
        Left err -> assertFailure $ show err
        Right assertion -> assertion

-- Can copy and paste from an imported module
unit_copy_paste_import :: Assertion
unit_copy_paste_import =
  let (prog, maxID) = create defaultImportProg
      a = newApp{appProg = prog}
      test = do
        prog' <- handleEditRequest [CreateDef Nothing]
        case (findGlobalByName prog' "other", Map.assocs $ moduleDefs $ progModule prog') of
          (Just (DefAST other), [(i, _)]) -> do
            let fromDef = astDefID other
                fromType = getID $ astDefType other
                fromExpr = getID $ astDefExpr other
            _ <-
              handleEditRequest
                [ MoveToDef i
                , CopyPasteSig (fromDef, fromType) []
                , CopyPasteBody (fromDef, fromExpr) []
                ]
            pure $ pure ()
          (Nothing, _) -> pure $ assertFailure "Could not find the imported 'other'"
          (Just _, _) -> pure $ assertFailure "Expected one def which was just created"
   in case fst $ runAppTestM maxID a test of
        Left err -> assertFailure $ show err
        Right assertion -> assertion

-- * Utilities

findGlobalByName :: Prog -> Name -> Maybe Def
findGlobalByName p n = find ((== n) . defName) $ concatMap (Map.elems . moduleDefs) $ progModule p : progImports p

-- We use a program with two defs: "main" and "other"
defaultEmptyProg :: MonadFresh ID m => m Prog
defaultEmptyProg = do
  mainExpr <- emptyHole
  mainType <- tEmptyHole
  otherExpr <- emptyHole
  otherType <- tEmptyHole
  let mainDef = ASTDef 0 "main" mainExpr mainType
      otherDef = ASTDef 2 "other" otherExpr otherType
   in pure $
        newEmptyProg
          { progSelection =
              Just $
                Selection mainDef $
                  Just
                    NodeSelection
                      { nodeType = BodyNode
                      , nodeId = 1
                      , meta = Left (Meta 1 Nothing Nothing)
                      }
          }
          & #progModule
          % #moduleDefs
          .~ Map.fromList [(0, DefAST mainDef), (2, DefAST otherDef)]

-- `defaultEmptyProg`, plus all primitive definitions (types and terms)
defaultPrimsProg :: MonadFresh ID m => m Prog
defaultPrimsProg = do
  p <- defaultEmptyProg
  withPrimDefs $ \_ m ->
    pure $
      over (#progModule % #moduleTypes) ((TypeDefPrim <$> toList allPrimTypeDefs) <>)
        . over (#progModule % #moduleDefs) ((DefPrim <$> m) <>)
        $ p

-- has `defaultPrimsProg` as an imported module, and a blank editable module
defaultImportProg :: MonadFresh ID m => m Prog
defaultImportProg = do
  p <- defaultPrimsProg
  pure
    p
      { progImports = [progModule p]
      , progModule = Module{moduleTypes = mempty, moduleDefs = mempty}
      }

_defIDs :: Traversal' ASTDef ID
_defIDs = #astDefID `adjoin` #astDefExpr % (_exprMeta % _id `adjoin` _exprTypeMeta % _id) `adjoin` #astDefType % _typeMeta % _id

-- Tests that the result is successful, and then applies the given test to the returned program
-- The test takes two arguments: the original input program and the output program
expectSuccess :: (Prog -> Prog -> Assertion) -> Prog -> Either ProgError Prog -> Assertion
expectSuccess f p = \case
  Right r -> f p r
  Left e -> assertFailure $ show e

expectError :: (ProgError -> Assertion) -> Prog -> Either ProgError Prog -> Assertion
expectError f _ = \case
  Right r -> assertFailure $ show r
  Left e -> f e

-- Run the given ProgActions against the given Prog, and pass the result to the given test function
progActionTest :: S Prog -> [ProgAction] -> (Prog -> Either ProgError Prog -> Assertion) -> Assertion
progActionTest inputProg actions testOutput = do
  let (prog, maxID) = create inputProg
  let a = newEmptyApp{appProg = prog}
  testOutput prog $ fst $ runAppTestM maxID a (handleEditRequest actions)

newtype AppTestM a = AppTestM {unAppTestM :: StateT App (ExceptT ProgError TestM) a}
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadFresh ID
    , MonadFresh NameCounter
    , MonadState App
    , MonadError ProgError
    )

runAppTestM :: ID -> App -> AppTestM a -> (Either ProgError a, App)
runAppTestM startID a m =
  case evalTestM startID $ runExceptT $ flip runStateT a $ unAppTestM m of
    Left err -> (Left err, a)
    Right (res, app') -> (Right res, app')
