{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
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
import Hooky.Utils.OsPath qualified as OsPath
import Hooky.Utils.Process (runProcessWith)
import Hooky.Utils.Text qualified as Text
import System.OsPath (OsPath, osp)
import UnliftIO.Exception (fromEitherM)

data GitClient = GitClient
  { repo :: OsPath
  }

-- | Initialize a git client at the current directory.
initGitClient :: IO GitClient
initGitClient = do
  let cwdClient =
        GitClient
          { repo = [osp|.|]
          }
  repo <- cwdClient.query ["rev-parse", "--show-toplevel"]
  pure
    GitClient
      { repo = OsPath.fromText repo
      }

instance HasField "run" GitClient ([String] -> IO (Either HookyError Text)) where
  getField git args = runProcessWith id "git" $ ["-C", OsPath.toFilePath git.repo] <> args
instance HasField "exec" GitClient ([String] -> IO ()) where
  getField git args = void . fromEitherM $ git.run args
instance HasField "query" GitClient ([String] -> IO Text) where
  getField git args = fmap Text.strip . fromEitherM $ git.run args

instance HasField "getPath" GitClient (OsPath -> IO OsPath) where
  getField git path = OsPath.fromText <$> git.query ["rev-parse", "--git-path", OsPath.toFilePath path]
instance HasField "getDiff" GitClient (IO Text) where
  getField git = git.query ["diff", "--no-ext-diff", "--no-textconv", "--ignore-submodules"]
instance HasField "clearChanges" GitClient (IO ()) where
  getField git = git.exec ["checkout", "--no-recurse-submodules", "--", "."]

-- | Get lines from the output of a git command that accepts "-z"
instance HasField "getLinesFrom" GitClient ([String] -> IO [Text]) where
  getField git args = Text.splitNULs <$> git.query (args <> ["-z"])
