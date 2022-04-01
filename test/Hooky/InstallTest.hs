{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Hooky.InstallTest (test) where

import Data.Text qualified as Text
import Path (relfile, (</>))
import Test.Tasty
import Test.Tasty.HUnit

import Hooky.Install (doInstall)
import Hooky.TestUtils (getPreCommitHookOutput, withGitRepo, withTestDir)
import Hooky.Utils.Path (writeScript)

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
        withGitRepo $ \repo ->
          withTestScript $ \script getRunArgs -> do
            doInstall repo script []
            args <- getRunArgs <$> getPreCommitHookOutput repo
            args @?= ["run"]
    , testCase "passes extra args to script" $
        withGitRepo $ \repo ->
          withTestScript $ \script getRunArgs -> do
            doInstall repo script ["--foo", "--bar", "a", "b", "c"]
            args <- getRunArgs <$> getPreCommitHookOutput repo
            args @?= ["run", "--foo", "--bar", "a", "b", "c"]
    , testCase "quotes extra args" $
        withGitRepo $ \repo ->
          withTestScript $ \script getRunArgs -> do
            doInstall repo script ["a b c", "d", "e f"]
            args <- getRunArgs <$> getPreCommitHookOutput repo
            args @?= ["run", "a b c", "d", "e f"]
    ]
  where
    withTestScript f =
      withTestDir $ \scriptDir -> do
        let script = scriptDir </> [relfile|test.sh|]
        writeScript script . Text.unlines $
          [ "#!/usr/bin/env sh"
          , "printf '%s\n' \"$@\""
          ]
        let getRunArgs = filter (/= "") . Text.splitOn "\n"
        f script getRunArgs

