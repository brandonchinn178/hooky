{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Hooky.TestUtils.Git (
  GitRepo (..),
  withGitRepo,
) where

import GHC.Records (HasField (..))
import System.Directory (
  createDirectoryIfMissing,
  withCurrentDirectory,
 )
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcessWithExitCode)
import UnliftIO.Exception (onException)

data GitRepo = GitRepo
  { repo :: FilePath
  , logfile :: FilePath
  }

instance HasField "run" GitRepo ([String] -> IO ()) where
  getField git args = do
    (code, stdout, stderr) <- readProcessWithExitCode "git" (["-C", git.repo] <> args) ""
    appendFile git.logfile stdout
    appendFile git.logfile stderr
    case code of
      ExitSuccess -> pure ()
      ExitFailure n -> do
        error $ "command exited with code " <> show n <> ": " <> show ("git" : args)
instance HasField "add" GitRepo ([FilePath] -> IO ()) where
  getField git files = git.run $ "add" : files
instance HasField "commit" GitRepo (String -> IO ()) where
  getField git msg = git.run ["commit", "-m", msg]
instance HasField "rm" GitRepo ([FilePath] -> IO ()) where
  getField git files = git.run $ "rm" : files

withGitRepo :: (GitRepo -> IO a) -> IO a
withGitRepo action =
  withSystemTempDirectory "git.XXXX" $ \tmpdir -> do
    let git =
          GitRepo
            { repo = tmpdir </> "repo"
            , logfile = tmpdir </> "git.log"
            }
    createDirectoryIfMissing True git.repo
    withCurrentDirectory git.repo . (`onException` outputLogs git.logfile) $ do
      git.run ["init", "--initial-branch", "main"]
      action git
 where
  outputLogs logFile = do
    putStrLn "\n===== Git output ====="
    putStrLn =<< readFile logFile
