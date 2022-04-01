{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Hooky.InstallTest (test) where

import Data.Text.Lazy qualified as TextL
import Data.Text.Lazy.Encoding qualified as TextL
import Path (relfile, toFilePath, (</>))
import Path.IO (getPermissions, setPermissions, setOwnerExecutable)
import System.Process.Typed (readProcess_)
import Test.Tasty
import Test.Tasty.HUnit

import Hooky.Git (git)
import Hooky.Install (doInstall)
import Hooky.TestUtils (withGitRepo, withTestDir)

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
    [ testCase "runs the given script on commit" $ do
        args <- doInstallAndGetArgs []
        args @?= ["run"]
    , testCase "passes extra args to script" $ do
        args <- doInstallAndGetArgs ["--foo", "--bar", "a", "b", "c"]
        args @?= ["run", "--foo", "--bar", "a", "b", "c"]
    , testCase "quotes extra args" $ do
        args <- doInstallAndGetArgs ["a b c", "d", "e f"]
        args @?= ["run", "a b c", "d", "e f"]
    ]
  where
    doInstallAndGetArgs extraRunArgs =
      withTestDir $ \scriptDir ->
        withGitRepo $ \repo -> do
          -- create a test script
          let script = scriptDir </> [relfile|test.sh|]
          writeFile (toFilePath script) . unlines $
            [ "#!/usr/bin/env sh"
            , "printf '%s\n' \"$@\""
            ]
          setPermissions script . setOwnerExecutable True =<< getPermissions script

          -- install it
          doInstall repo script extraRunArgs

          -- run a commit
          (_, stderr) <- readProcess_ $ git repo ["commit", "--allow-empty", "-m", "test commit"]
          return $ filter (/= "") $ TextL.splitOn "\n" $ TextL.decodeUtf8 stderr
