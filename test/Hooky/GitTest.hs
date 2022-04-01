{-# LANGUAGE QuasiQuotes #-}

module Hooky.GitTest (test) where

import Path (reldir, relfile, toFilePath, (</>))
import Path.IO (ensureDir)
import Test.Tasty
import Test.Tasty.HUnit

import Hooky.Git (fromGitRepo, getGitPath, getGitRepo, git_, unsafeMakeGitRepo)
import Hooky.TestUtils (withGitRepo, withTestDir)

test :: TestTree
test =
  testGroup
    "Hooky.Git"
    [ testGetGitRepo
    , testGetGitPath
    ]

testGetGitRepo :: TestTree
testGetGitRepo =
  testGroup
    "getGitRepo"
    [ testCase "returns Nothing when called on a non-git directory" $
        withTestDir $ \dir -> do
          res <- getGitRepo dir
          res @?= Nothing
    , testCase "returns same directory when called on a path to a git repo" $
        withGitRepo $ \repo -> do
          res <- getGitRepo (fromGitRepo repo)
          res @?= Just repo
    , testCase "returns git directory when called on a subdirectory in a git repo" $
        withGitRepo $ \repo -> do
          let subdir = fromGitRepo repo </> [reldir|foo/bar/|]
          ensureDir subdir
          res <- getGitRepo subdir
          res @?= Just repo
    ]

testGetGitPath :: TestTree
testGetGitPath =
  testGroup
    "getGitPath"
    [ testCase "returns file in git directory" $
        withGitRepo $ \repo -> do
          res <- getGitPath repo [relfile|foo/bar.txt|]
          res @?= (fromGitRepo repo </> [relfile|.git/foo/bar.txt|])
    , testCase "returns directory in git directory" $
        withGitRepo $ \repo -> do
          res <- getGitPath repo [reldir|foo/|]
          res @?= (fromGitRepo repo </> [reldir|.git/foo/|])
    , testCase "returns file in worktree git directory when called in a worktree" $
        withGitWorktree $ \repo worktreeRepo -> do
          res <- getGitPath worktreeRepo [relfile|foo/bar.txt|]
          res @?= (fromGitRepo repo </> [relfile|.git/worktrees/test/foo/bar.txt|])
    , testCase "returns directory in worktree git directory when called in a worktree" $
        withGitWorktree $ \repo worktreeRepo -> do
          res <- getGitPath worktreeRepo [reldir|foo/|]
          res @?= (fromGitRepo repo </> [reldir|.git/worktrees/test/foo/|])
    , testCase "returns common path in worktree git directory when called in a worktree" $
        withGitWorktree $ \repo worktreeRepo -> do
          res <- getGitPath worktreeRepo [relfile|info/exclude|]
          res @?= (fromGitRepo repo </> [relfile|.git/info/exclude|])
    ]
  where
    withGitWorktree f =
      withTestDir $ \dir ->
        withGitRepo $ \repo -> do
          let worktreeDir = dir </> [reldir|test|]
          git_ repo ["worktree", "add", "-b", "test-branch", toFilePath worktreeDir]
          f repo (unsafeMakeGitRepo worktreeDir)
