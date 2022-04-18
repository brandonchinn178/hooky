{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Hooky.RunTest (test) where

import Data.Text qualified as Text
import Path (parseAbsDir, relfile, toFilePath, (</>))
import System.IO.Silently qualified as Silently
import Test.Tasty
import Test.Tasty.HUnit

import Hooky.Config (Check (..), Config (..))
import Hooky.Install (doInstall)
import Hooky.Run (RunOptions (..), doRun)
import Hooky.TestUtils (getPreCommitHookOutput, withGitRepo, withTestDir)
import Hooky.Utils.Git (git_, inRepo)
import Paths_hooky (getBinDir)

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
    [ testCase "works with commit -a" $
        withGitRepo $ \repo ->
          withTestDir $ \tmpdir -> do
            -- install hooky to repo with test config
            bindir <- getBinDir >>= parseAbsDir
            let hookyExe = bindir </> [relfile|hooky|]
            let configFile = toFilePath $ tmpdir </> [relfile|hooky-config.yaml|]
            writeFile configFile . unlines $
              [ "checks:"
              , "- name: test"
              , "  command: echo"
              ]
            doInstall repo hookyExe ["-c", Text.pack configFile, "--verbose"]
            let getCheckedFiles = tail . Text.lines

            -- committing explicitly staged file works
            let file = toFilePath $ inRepo repo [relfile|a.txt|]
            writeFile file ""
            git_ repo ["add", "a.txt"]
            out1 <- getCheckedFiles <$> getPreCommitHookOutput repo []
            out1 @?= ["a.txt"]

            -- committing modified file with "-a" works
            writeFile file "edit"
            out2 <- getCheckedFiles <$> getPreCommitHookOutput repo ["--all"]
            out2 @?= ["a.txt"]
    , testCase "works when specifying a single command" $
        withGitRepo $ \repo -> do
          getRunResult repo basicConfig
    -- , testCase "works when specifying a shell command"
    -- , testCase "works when specifying a shell command with explicit $@"
    -- , testCase "works when specifying a command list"
    -- , testCase "shows stdout on success when configured"
    ]

getRunResult :: GitRepo -> Config -> IO (String, Bool)
getRunResult repo config = getRunResult' repo config testRunOptions

getRunResult' :: GitRepo -> Config -> RunOptions -> IO (String, Bool)
getRunResult' repo config opts = Silently.capture $ doRun repo config opts

testRunOptions :: RunOptions
testRunOptions =
  RunOptions
    { showStdoutOnSuccess = False
    }

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
