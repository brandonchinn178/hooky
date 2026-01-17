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
import Hooky.Utils.Process (runProcessWith)
import UnliftIO.Exception (fromEitherM)

data GitClient = GitClient
  { repo :: FilePath
  }

-- | Initialize a git client at the current directory.
initGitClient :: IO GitClient
initGitClient = do
  repo <- (GitClient ".").query ["rev-parse", "--show-toplevel"]
  pure
    GitClient
      { repo = Text.unpack repo
      }

instance HasField "run" GitClient ([String] -> IO (Either HookyError Text)) where
  getField git args = runProcessWith id "git" $ ["-C", git.repo] <> args
instance HasField "exec" GitClient ([String] -> IO ()) where
  getField git args = void . fromEitherM $ git.run args
instance HasField "query" GitClient ([String] -> IO Text) where
  getField git args = fmap Text.strip . fromEitherM $ git.run args

instance HasField "getPath" GitClient (FilePath -> IO Text) where
  getField git path = git.query ["rev-parse", "--git-path", path]
instance HasField "getDiff" GitClient (IO Text) where
  getField git = git.query ["diff", "--no-ext-diff", "--no-textconv", "--ignore-submodules"]
instance HasField "clearChanges" GitClient (IO ()) where
  getField git = git.exec ["checkout", "--no-recurse-submodules", "--", "."]

instance HasField "getFilesWith" GitClient ([String] -> IO [FilePath]) where
  getField git args = split <$> git.query (args <> ["-z"])
   where
    split = map Text.unpack . filter (not . Text.null) . Text.splitOn (Text.pack "\0")
instance HasField "getFiles" GitClient (IO [FilePath]) where
  getField git = git.getFilesWith ["ls-files"]
instance HasField "getChangedFiles" GitClient ([String] -> IO [FilePath]) where
  getField git args = git.getFilesWith $ ["diff", "--name-only", "--diff-filter=AMR"] <> args
