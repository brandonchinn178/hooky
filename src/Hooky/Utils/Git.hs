{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Hooky.Utils.Git (
  GitClient (..),
  initGitClient,
) where

import Control.Monad (void)
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Records (HasField (..))
import Hooky.Error (HookyError (..))
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)
import UnliftIO.Exception (fromEitherM)

data GitClient = GitClient
  { repo :: FilePath
  }

-- | Initialize a git client at the current directory.
initGitClient :: IO GitClient
initGitClient = do
  repo <- fromEitherM $ runGit ["rev-parse", "--show-toplevel"]
  pure
    GitClient
      { repo = Text.unpack repo
      }

runGit :: [String] -> IO (Either HookyError Text)
runGit args = do
  (code, stdout, stderr) <- readProcessWithExitCode "git" args ""
  pure $
    case code of
      ExitSuccess ->
        Right $ (Text.strip . Text.pack) stdout
      ExitFailure n ->
        Left . HookyError . Text.unlines $
          [ Text.pack $ "command exited with code " <> show n <> ": " <> show ("git" : args)
          , Text.pack stderr
          ]

instance HasField "run" GitClient ([String] -> IO (Either HookyError Text)) where
  getField git args = runGit $ ["-C", git.repo] <> args
instance HasField "exec" GitClient ([String] -> IO ()) where
  getField git args = void . fromEitherM $ git.run args
instance HasField "query" GitClient ([String] -> IO Text) where
  getField git args = fromEitherM $ git.run args
instance HasField "getPath" GitClient (FilePath -> IO Text) where
  getField git path = git.query ["rev-parse", "--git-path", path]
