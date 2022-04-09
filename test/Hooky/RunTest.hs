module Hooky.RunTest (test) where

import Test.Tasty

test :: TestTree
test =
  testGroup
    "Hooky.Run"
    [ testDoRun
    ]

-- TODO: test commit -a, running various command formats
testDoRun :: TestTree
testDoRun =
  testGroup
    "doRun"
    []
