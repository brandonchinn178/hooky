{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Hooky.InstallTest (test) where

import Path (reldir, relfile, toFilePath, (</>))
import Path.IO (getPermissions, setPermissions, setOwnerExecutable)
import System.Process.Typed (readProcess_)
import Test.Tasty
import Test.Tasty.HUnit

import Hooky.Git (git)
import Hooky.Install (doInstall)
import Hooky.TestUtils (withGitDir, withTestDir)

test :: TestTree
test =
  testGroup
    "Hooky.Install"
    [ testDoInstall
    ]

testDoInstall :: TestTree
testDoInstall =
  testGroup
    "doInstall"
    [ testCase "runs the given script on commit" $
        withTestDir $ \scriptDir ->
          withGitDir $ \dir -> do
            -- create a test script
            let script = scriptDir </> [relfile|test.sh|]
            writeFile (toFilePath script) . unlines $
              [ "#!/usr/bin/env sh"
              , "echo \"Ran with args: $*\""
              ]
            setPermissions script . setOwnerExecutable True =<< getPermissions script

            -- install it
            doInstall (dir </> [reldir|.git|]) script

            -- run a commit
            (_, stderr) <- readProcess_ $ git dir ["commit", "--allow-empty", "-m", "test commit"]
            stderr @?= "Ran with args: run\n"
    ]
