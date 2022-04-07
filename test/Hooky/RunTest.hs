module Hooky.RunTest (test) where

import Test.Tasty

test :: TestTree
test =
  testGroup
    "Hooky.Run"
    [ testDoRun
    ]

testDoRun :: TestTree
testDoRun =
  testGroup
    "doRun"
    [ -- TODO: test commit -a
    ]

