{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Hooky.GitTest (test) where

import Path (reldir, toFilePath, (</>))
import Path.IO (ensureDir, withSystemTempDir)
import System.Process.Typed (proc, readProcess_)
import Test.Tasty
import Test.Tasty.HUnit

import Hooky.Git (getGitRoot)

test :: TestTree
test =
  testGroup
    "Hooky.Git"
    [ testGetGitRoot
    ]

testGetGitRoot :: TestTree
testGetGitRoot =
  testGroup
    "getGitRoot"
    [ testCase "returns Nothing when called on a non-git directory" $
        withTestDir $ \dir -> do
          res <- getGitRoot dir
          res @?= Nothing
    , testCase "returns git directory when called on a path to a git repo" $
        withGitDir $ \dir -> do
          res <- getGitRoot dir
          res @?= Just (dir </> [reldir|.git|])
    , testCase "returns git directory when called on a subdirectory in a git repo" $
        withGitDir $ \dir -> do
          let subdir = dir </> [reldir|foo/bar/|]
          ensureDir subdir
          res <- getGitRoot subdir
          res @?= Just (dir </> [reldir|.git|])
    , testCase "returns original git directory when called on a path to a git worktree" $
        withTestDir $ \worktree ->
          withGitDir $ \dir -> do
            git dir ["worktree", "add", "-b", "test-branch", toFilePath worktree]
            res <- getGitRoot worktree
            res @?= Just (dir </> [reldir|.git|])
    , testCase "returns original git directory when called on a subdirectory in a git worktree" $
        withTestDir $ \worktree ->
          withGitDir $ \dir -> do
            git dir ["worktree", "add", "-b", "test-branch", toFilePath worktree]
            let subdir = worktree </> [reldir|foo/bar/|]
            ensureDir subdir
            res <- getGitRoot subdir
            res @?= Just (dir </> [reldir|.git|])
    ]
  where
    withTestDir = withSystemTempDir "getGitRoot-test"
    withGitDir f =
      withTestDir $ \dir -> do
        git dir ["init"]
        git dir ["commit", "--allow-empty", "-m", "Initial commit"]
        f dir
    git dir args = do
      _ <- readProcess_ $ proc "git" $ ["-C", toFilePath dir] <> args
      return ()
