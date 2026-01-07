{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Hooky.TestUtils.Git (
  TestGitClient (..),
  withGitRepo,
) where

import Control.Monad (unless)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import GHC.Records (HasField (..))
import Hooky.Utils.Git (GitClient (..))
import System.Directory (
  createDirectoryIfMissing,
  withCurrentDirectory,
 )
import System.FilePath ((</>))
import System.IO qualified as IO
import System.IO.Temp (withSystemTempDirectory)

newtype TestGitClient = TestGitClient GitClient

instance HasField "client" TestGitClient GitClient where
  getField (TestGitClient client) = client
instance HasField "exec" TestGitClient ([String] -> IO ()) where
  getField git args = do
    log_ $ "exec: " <> (Text.pack . show) args
    stdout <- git.client.query args
    unless (Text.null stdout) $ do
      log_ $ "stdout:\n" <> stdout
   where
    log_ msg = Text.hPutStrLn IO.stderr $ "[TestGitClient] " <> msg

withGitRepo :: (TestGitClient -> IO a) -> IO a
withGitRepo action =
  withSystemTempDirectory "git.XXXX" $ \tmpdir -> do
    let git =
          TestGitClient
            GitClient
              { repo = tmpdir </> "repo"
              }
    createDirectoryIfMissing True git.client.repo
    withCurrentDirectory git.client.repo $ do
      git.exec ["init", "--initial-branch", "main"]
      action git
