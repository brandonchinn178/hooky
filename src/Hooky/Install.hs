{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Hooky.Install (
  doInstall,
) where

import Data.Text (Text)
import Data.Text qualified as Text
import Path (Abs, Dir, File, Path, relfile, toFilePath, (</>))
import Path.IO (getPermissions, setPermissions, setOwnerExecutable)

doInstall ::
  -- | The path to the root of the git repository
  Path Abs Dir ->
  -- | The path to the 'hooky' executable to install
  Path Abs File ->
  -- | Extra arguments to pass to the 'hooky run' command
  [Text] ->
  IO ()
doInstall gitDir hookyExe extraRunArgs = do
  writeFile (toFilePath preCommitPath) . unlines $
    [ "#!/usr/bin/env sh"
    , "exec " <> toFilePath hookyExe <> " run " <> Text.unpack args
    ]
  setPermissions preCommitPath . setOwnerExecutable True =<< getPermissions preCommitPath
  where
    preCommitPath = gitDir </> [relfile|hooks/pre-commit|]
    args = Text.unwords $ map (\s -> "'" <> s <> "'") extraRunArgs
