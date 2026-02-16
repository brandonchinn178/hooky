{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Hooky.TestUtils.Git (
  TestGitClient (..),
  withGitRepo,
) where

import Control.Monad (unless)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import GHC.Records (HasField (..))
import Hooky.Utils.Git (GitClient (..))
import System.Directory (
  createDirectoryIfMissing,
  withCurrentDirectory,
 )
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO qualified as IO
import System.IO.Temp (withSystemTempDirectory)
import System.Process qualified as Process

data TestGitClient = TestGitClient {repo :: FilePath}

instance HasField "client" TestGitClient GitClient where
  getField TestGitClient{repo} = GitClient{repo}

instance HasField "run" TestGitClient ([String] -> IO (ExitCode, Text, Text)) where
  getField git args = do
    log_ $ "run: " <> (Text.pack . show) args
    (code, stdoutS, stderrS) <-
      flip Process.readCreateProcessWithExitCode "" $
        Process.proc "git" $
          ["-C", git.repo] <> args
    let (stdout, stderr) = (toText stdoutS, toText stderrS)
    unless (Text.null stdout) $ do
      log_ $ "stdout:\n" <> stdout
    unless (Text.null stderr) $ do
      log_ $ "stderr:\n" <> stderr
    pure (code, stdout, stderr)
   where
    toText = Text.strip . Text.pack
    log_ msg = Text.hPutStrLn IO.stderr $ "[TestGitClient] " <> msg

instance HasField "exec" TestGitClient ([String] -> IO ()) where
  getField git args = do
    (code, _, _) <- git.run args
    case code of
      ExitSuccess -> pure ()
      ExitFailure n -> error $ "git failed with " <> show n

withGitRepo :: (TestGitClient -> IO a) -> IO a
withGitRepo action =
  withSystemTempDirectory "git.XXXX" $ \tmpdir -> do
    let git = TestGitClient{repo = tmpdir </> "repo"}
    createDirectoryIfMissing True git.repo
    withCurrentDirectory git.repo $ do
      git.exec ["init", "--initial-branch", "main"]
      action git
