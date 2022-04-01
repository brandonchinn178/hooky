{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Hooky.InstallTest (test) where

import Data.Text qualified as Text
import Path (relfile, toFilePath, (</>))
import Path.IO (doesFileExist)
import Test.Tasty
import Test.Tasty.HUnit

import Hooky.Install (doInstall)
import Hooky.TestUtils (getPreCommitHookOutput, withGitRepo, withTestDir)
import Hooky.Utils.Git (fromGitRepo, git_)
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
    , testCase "respects core.hooksPath" $ do
        withTestDir $ \hooksDir ->
          withGitRepo $ \repo ->
            withTestScript $ \script getRunArgs -> do
              git_ repo ["config", "core.hooksPath", toFilePath hooksDir]
              doInstall repo script []

              -- puts it in right place
              hookInGitDirExists <- doesFileExist $ fromGitRepo repo </> [relfile|.git/hooks/pre-commit|]
              hookInGitDirExists @?= False
              hookInHooksDirExists <- doesFileExist $ hooksDir </> [relfile|pre-commit|]
              hookInHooksDirExists @?= True

              -- verify it runs correctly
              args <- getRunArgs <$> getPreCommitHookOutput repo
              args @?= ["run"]
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

