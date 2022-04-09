{-# LANGUAGE QuasiQuotes #-}

module Hooky.Utils.GitTest (test) where

import Path (reldir, relfile, toFilePath, (</>))
import Path.IO (ensureDir)
import Test.Tasty
import Test.Tasty.HUnit

import Hooky.TestUtils (withGitRepo, withTestDir)
import Hooky.Utils.Git (
  fromGitRepo,
  getGitPath,
  getGitRepo,
  getStagedFiles,
  git_,
  inRepo,
  unsafeMakeGitRepo,
 )

test :: TestTree
test =
  testGroup
    "Hooky.Git"
    [ testGetGitRepo
    , testGetGitPath
    , testGetStagedFiles
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

testGetStagedFiles :: TestTree
testGetStagedFiles =
  testGroup
    "getStagedFiles"
    [ testCase "gets staged files" $
        withGitRepo $ \repo -> do
          let file1 = [relfile|file1.txt|]
              file2 = [relfile|file2.txt|]
              file3 = [relfile|file3.txt|]
          create repo [file1, file2, file3]
          stage repo [file1, file2]
          files <- getStagedFiles repo
          files @?= [file1, file2]
    , testCase "returns an empty list when no staged files" $
        withGitRepo $ \repo -> do
          let file1 = [relfile|file1.txt|]
              file2 = [relfile|file2.txt|]
              file3 = [relfile|file3.txt|]
          create repo [file1, file2, file3]
          files <- getStagedFiles repo
          files @?= []
    , testCase "handles files with whitespace" $
        withGitRepo $ \repo -> do
          let file = [relfile|a b c.txt|]
          create repo [file]
          stage repo [file]
          files <- getStagedFiles repo
          files @?= [file]
    , testCase "ignores deleted files" $
        withGitRepo $ \repo -> do
          let file1 = [relfile|file1.txt|]
              file2 = [relfile|file2.txt|]
          create repo [file1]
          stage repo [file1]
          git_ repo ["commit", "-m", "initial commit"]
          git_ repo ["rm", toFilePath file1]
          create repo [file2]
          stage repo [file2]
          files <- getStagedFiles repo
          files @?= [file2]
    ]
  where
    create repo = mapM_ (flip writeFile "" . toFilePath . inRepo repo)
    stage repo files = git_ repo $ ["add"] <> map toFilePath files
