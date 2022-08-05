module Tests.OpenAPI where

import Foreword

import Data.Aeson.Encode.Pretty (encodePretty)
import Primer.Server (openAPIInfo)
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
