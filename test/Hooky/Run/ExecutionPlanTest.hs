{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Hooky.Run.ExecutionPlanTest (test) where

import Data.List.NonEmpty qualified as NonEmpty
import Path (File, Path, Rel, relfile)
import Test.Tasty
import Test.Tasty.HUnit

import Hooky.Config (Check (..), CommandDefinition (..), Config (..))
import Hooky.Run.ExecutionPlan (ExecutionPlan (..), ExecutionStep (..), compilePlan)

test :: TestTree
test =
  testGroup
    "Hooky.Run.ExecutionPlan"
    [ testCompilePlan
    ]

-- TODO: test ExecutionCommand from CommandDefinition
testCompilePlan :: TestTree
testCompilePlan =
  testGroup
    "compilePlan"
    [ testCase "filters out files matching exclude" $ do
        let ExecutionStep{stepFiles} =
              compileSingleStep
                basicConfig{cfgExclude = ["*.txt"]}
                [ [relfile|readme.md|]
                , [relfile|foo.txt|]
                ]
        assertBool "foo.txt was unexpectedly in the plan" $
          [relfile|foo.txt|] `notElem` stepFiles
    , testCase "only includes files matching patterns" $ do
        let ExecutionStep{stepFiles} =
              compileSingleStep
                basicConfig{cfgChecks = [basicCheck{checkFiles = ["*.txt"]}]}
                [ [relfile|readme.md|]
                , [relfile|foo.txt|]
                ]
        stepFiles @?= NonEmpty.singleton [relfile|foo.txt|]
    ]

compileSingleStep :: Config -> [Path Rel File] -> ExecutionStep
compileSingleStep config files =
  case compilePlan config files of
    ExecutionPlan [step] -> step
    ExecutionPlan steps -> error $ "Unexpected plan: " ++ show steps

basicConfig :: Config
basicConfig =
  Config
    { cfgChecks = [basicCheck]
    , cfgSources = mempty
    , cfgExclude = []
    }

basicCheck :: Check
basicCheck =
  Check
    { checkName = "test"
    , checkCommand = ExplicitCommand "true"
    , checkFiles = []
    }
