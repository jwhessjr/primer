module Tests.OpenAPI where

import Foreword

import Data.Aeson (ToJSON)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.OpenApi (ToSchema, validatePrettyToJSON)
import Data.UUID (UUID, fromWords64)
import Hedgehog (Gen, annotate, failure, forAll)
import Hedgehog.Gen qualified as G
import Hedgehog.Range qualified as R
import Primer.API (
  Def (Def),
  Module (Module),
  NodeBody (BoxBody, NoBody, TextBody),
  NodeFlavor,
  Prog (Prog),
  Tree,
  viewTreeExpr,
  viewTreeType,
 )
import Primer.Core (ID (ID))
import Primer.Database (Session (Session), SessionName, safeMkSessionName)
import Primer.Gen.Core.Raw (
  ExprGen,
  evalExprGen,
  genExpr,
  genGVarName,
  genLVarName,
  genModuleName,
  genName,
  genTyConName,
  genType,
  genValConName,
 )
import Primer.OpenAPI ()
import Primer.Pagination (NonNeg, Positive, mkNonNeg, mkPositive)
import Primer.Server (openAPIInfo)
import Tasty (Property, property)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)

-- Note: the golden output can be generated by running
--
-- @make openapi.json@
--
-- from the project's top-level directory and copying the output to
-- the path below.
test_golden :: TestTree
test_golden =
  testGroup
    "golden"
    [ goldenVsString "openapi.json" "test/outputs/OpenAPI/openapi.json" $
        pure $
          encodePretty openAPIInfo
    ]

testToJSON :: (ToJSON a, ToSchema a, Show a) => Gen a -> Property
testToJSON g = property $ do
  x <- forAll g
  case validatePrettyToJSON x of
    Nothing -> pure ()
    Just errs -> annotate errs >> failure

genSessionName :: Gen SessionName
genSessionName = safeMkSessionName <$> G.text (R.linear 1 100) G.unicode

tasty_SessionName :: Property
tasty_SessionName = testToJSON genSessionName

genUUID :: Gen UUID
genUUID = fromWords64 <$> G.word64 R.linearBounded <*> G.word64 R.linearBounded

genSession :: Gen Session
genSession = Session <$> genUUID <*> genSessionName

tasty_Session :: Property
tasty_Session = testToJSON genSession

-- NB: don't want to use genID, as that is just "next free ID"
tasty_ID :: Property
tasty_ID = testToJSON $ ID <$> G.int (R.linear 0 1000)

tasty_Name :: Property
tasty_Name = testToJSON $ evalExprGen 0 genName

tasty_ModuleName :: Property
tasty_ModuleName = testToJSON $ evalExprGen 0 genModuleName

tasty_TyConName :: Property
tasty_TyConName = testToJSON $ evalExprGen 0 genTyConName

tasty_ValConName :: Property
tasty_ValConName = testToJSON $ evalExprGen 0 genValConName

tasty_GVarName :: Property
tasty_GVarName = testToJSON $ evalExprGen 0 genGVarName

tasty_LVarName :: Property
tasty_LVarName = testToJSON genLVarName

tasty_Tree :: Property
tasty_Tree = testToJSON genTree

-- We only test the trees which we create by viewing either a Type or Expr
genTree :: Gen Tree
genTree = evalExprGen 0 $ G.choice [genExprTree, genTypeTree]

genExprTree :: ExprGen Tree
genExprTree = viewTreeExpr <$> genExpr

genTypeTree :: ExprGen Tree
genTypeTree = viewTreeType <$> genType

tasty_NodeBody :: Property
tasty_NodeBody =
  testToJSON $
    G.choice
      [ TextBody <$> G.text (R.linear 1 20) G.unicode
      , BoxBody <$> genTree
      , pure NoBody
      ]

tasty_NodeFlavor :: Property
tasty_NodeFlavor = testToJSON $ G.enumBounded @_ @NodeFlavor

genDef :: ExprGen Def
genDef = Def <$> genGVarName <*> genExprTree <*> G.maybe genTypeTree

tasty_Def :: Property
tasty_Def = testToJSON $ evalExprGen 0 genDef

genModule :: ExprGen Module
genModule =
  Module
    <$> genModuleName
    <*> G.bool
    <*> G.list (R.linear 0 3) genTyConName
    <*> G.list (R.linear 0 3) genDef

tasty_Module :: Property
tasty_Module = testToJSON $ evalExprGen 0 genModule

tasty_Prog :: Property
tasty_Prog = testToJSON $ evalExprGen 0 $ Prog <$> G.list (R.linear 0 3) genModule

genPositive :: Gen Positive
genPositive = G.just $ mkPositive <$> G.int (R.linear 1 1000)

tasty_Positive :: Property
tasty_Positive = testToJSON genPositive

genNonNeg :: Gen NonNeg
genNonNeg = G.just $ mkNonNeg <$> G.int (R.linear 0 1000)

tasty_NonNeg :: Property
tasty_NonNeg = testToJSON genNonNeg
