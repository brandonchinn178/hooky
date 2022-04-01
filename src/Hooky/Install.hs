{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Hooky.Install (
  doInstall,
) where

import Data.Text (Text)
import Data.Text qualified as Text
import Path (Abs, File, Path, relfile, toFilePath)
import Path.IO (getPermissions, setPermissions, setOwnerExecutable)

import Hooky.Git (GitRepo, getGitPath)

doInstall ::
  -- | The path to the git repository
  GitRepo ->
  -- | The path to the 'hooky' executable to install
  Path Abs File ->
  -- | Extra arguments to pass to the 'hooky run' command
  [Text] ->
  IO ()
doInstall repo hookyExe extraRunArgs = do
  preCommitPath <- getGitPath repo [relfile|hooks/pre-commit|]
  writeFile (toFilePath preCommitPath) . unlines $
    [ "#!/usr/bin/env sh"
    , "exec " <> toFilePath hookyExe <> " run " <> Text.unpack args
    ]
  setPermissions preCommitPath . setOwnerExecutable True =<< getPermissions preCommitPath
  where
    args = Text.unwords $ map (\s -> "'" <> s <> "'") extraRunArgs
