{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Hooky.RunTest (test) where

import Data.Text qualified as Text
import Path (parseAbsDir, relfile, toFilePath, (</>))
import Test.Tasty
import Test.Tasty.HUnit

import Hooky.Install (doInstall)
import Hooky.TestUtils (getPreCommitHookOutput, withGitRepo, withTestDir)
import Hooky.Utils.Git (git_, inRepo)
import Paths_hooky (getBinDir)

test :: TestTree
test =
  testGroup
    "Hooky.Run"
    [ testDoRun
    ]

-- TODO: running various command formats
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
    ]
