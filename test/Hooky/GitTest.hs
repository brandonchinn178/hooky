{-# LANGUAGE QuasiQuotes #-}

module Hooky.GitTest (test) where

import Path (reldir, toFilePath, (</>))
import Path.IO (ensureDir)
import Test.Tasty
import Test.Tasty.HUnit

import Hooky.Git (getGitRoot, git_)
import Hooky.TestUtils (withGitDir, withTestDir)

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
            git_ dir ["worktree", "add", "-b", "test-branch", toFilePath worktree]
            res <- getGitRoot worktree
            res @?= Just (dir </> [reldir|.git|])
    , testCase "returns original git directory when called on a subdirectory in a git worktree" $
        withTestDir $ \worktree ->
          withGitDir $ \dir -> do
            git_ dir ["worktree", "add", "-b", "test-branch", toFilePath worktree]
            let subdir = worktree </> [reldir|foo/bar/|]
            ensureDir subdir
            res <- getGitRoot subdir
            res @?= Just (dir </> [reldir|.git|])
    ]
