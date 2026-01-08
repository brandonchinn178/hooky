{-# LANGUAGE OrPatterns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module Hooky.Run (
  -- * runHooks
  RunOptions (..),
  runHooks,

  -- * FileTargets
  FileTargets (..),

  -- * RunMode
  RunMode (..),
  allRunModes,
  parseRunMode,
  renderRunMode,
) where

import Control.Monad (forM_, unless)
import Data.ByteString.Lazy qualified as ByteStringL
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import GHC.Records (HasField (..))
import Hooky.Config (Config, HookConfig, PassFilesMode (..), matchesGlobs)
import Hooky.Config qualified as Config (Config (..))
import Hooky.Config qualified as HookConfig (HookConfig (..))
import Hooky.Utils.Git (GitClient)
import System.Exit (ExitCode (..), exitFailure)
import System.IO qualified as IO
import UnliftIO.Process qualified as Process
import UnliftIO.Temporary (withSystemTempFile)

{----- runHooks -----}

data RunOptions = RunOptions
  { mode :: RunMode
  , fileTargets :: FileTargets
  , showStdoutOnSuccess :: Bool
  , stash :: Bool
  }

instance HasField "autofix" RunOptions Bool where
  getField options =
    case options.mode of
      Mode_Fix
      Mode_FixAdd -> True
      Mode_Check -> False

runHooks :: GitClient -> Config -> RunOptions -> IO ()
runHooks git config options = do
  (if options.stash then withStash else id) $ do
    files <- resolveTargets git options.fileTargets
    let hooks = map (resolveHook config options files) config.hooks
    results <-
      case options.mode of
        Mode_Check -> do
          -- FIXME: run in parallel, stream output
          mapM run hooks <* checkModified
        Mode_Fix -> do
          mapM run hooks <* checkModified
        Mode_FixAdd -> do
          mapM run hooks <* stageModified
    printSummary results
    unless (all ((== HookPassed) . snd) results) $ do
      exitFailure
 where
  withStash = id -- FIXME
  checkModified = pure () -- FIXME: error if files modified
  stageModified = pure () -- FIXME: stage modified files
  run hook = do
    Text.putStrLn $ "Running: " <> hook.name
    (out_r, out_w) <- Process.createPipe
    result <- runHook hook out_w
    case result of
      HookFailed -> do
        Text.putStrLn $ hook.name <> ": FAIL"
        Text.putStrLn $ Text.replicate 80 "-"
        ByteStringL.hGetContents out_r >>= ByteStringL.putStr
        Text.putStrLn $ Text.replicate 80 "-"
      _ -> pure ()
    pure (hook.name, result)

{----- HookCmd -----}

data HookCmd = HookCmd
  { name :: Text
  , args :: NonEmpty Text
  , passFiles :: PassFilesMode
  , files :: [FilePath]
  }
  deriving (Show, Eq)

resolveHook :: Config -> RunOptions -> [FilePath] -> HookConfig -> HookCmd
resolveHook config options files hookConfig =
  HookCmd
    { name = hookConfig.name
    , args =
        if options.autofix
          then hookConfig.cmdArgs `NonEmpty.appendList` hookConfig.fixArgs
          else hookConfig.cmdArgs `NonEmpty.appendList` hookConfig.checkArgs
    , passFiles = hookConfig.passFiles
    , files = filter isIncluded files
    }
 where
  isIncluded fp = matchesGlobs (config.files <> hookConfig.files) (Text.pack fp)

runHook :: HookCmd -> IO.Handle -> IO HookResult
runHook hook outHandle = do
  if null hook.files
    then pure HookSkipped
    else do
      code <-
        case hook.passFiles of
          PassFiles_None -> run hook.args
          PassFiles_XArgs -> xargs []
          PassFiles_XArgsParallel -> xargs ["-P0"]
          PassFiles_File ->
            withSystemTempFile ("hooky." <> Text.unpack hook.name <> ".XXXXX") $ \fp h -> do
              mapM_ (IO.hPutStrLn h) hook.files
              IO.hClose h
              run $ hook.args `NonEmpty.appendList` [Text.pack $ '@' : fp]
      pure $
        case code of
          ExitSuccess -> HookPassed
          ExitFailure _ -> HookFailed
 where
  run (cmd NonEmpty.:| args) = runWithInput cmd args $ \_ -> pure ()
  xargs xargsArgs =
    runWithInput "xargs" (["-0"] <> xargsArgs <> NonEmpty.toList hook.args) $ \h ->
      forM_ hook.files $ \fp -> do
        IO.hPutStr h fp
        IO.hPutChar h '\0'

  runWithInput cmd args populateStdin = do
    (stdin_r, stdin_w) <- Process.createPipe
    Process.withCreateProcess
      (Process.proc (Text.unpack cmd) (map Text.unpack args))
        { Process.std_in = Process.UseHandle stdin_r
        , Process.std_out = Process.UseHandle outHandle
        , Process.std_err = Process.UseHandle outHandle
        , Process.close_fds = True
        }
      ( \_ _ _ h -> do
          populateStdin stdin_w :: IO ()
          IO.hClose stdin_w
          Process.waitForProcess h
      )

{----- HookResult -----}

data HookResult = HookPassed | HookFailed | HookSkipped
  deriving (Show, Eq)

printSummary :: [(Text, HookResult)] -> IO ()
printSummary results = do
  unless (null $ passed <> failed) $ do
    Text.putStrLn $ Text.replicate 80 "-"
  unless (null passed) $ do
    Text.putStrLn $ "Passed: " <> Text.intercalate ", " passed
  unless (null failed) $ do
    Text.putStrLn $ "Failed: " <> Text.intercalate ", " failed
 where
  passed = [hook | (hook, HookPassed) <- results]
  failed = [hook | (hook, HookFailed) <- results]

{----- FileTargets -----}

data FileTargets
  = FilesGiven [FilePath]
  | FilesModified
  | FilesStaged
  | FilesAll
  | FilesPrev
  deriving (Show, Eq)

resolveTargets :: GitClient -> FileTargets -> IO [FilePath]
resolveTargets git = \case
  FilesGiven files -> pure files
  FilesModified -> git.getChangedFiles []
  FilesStaged -> git.getChangedFiles ["--staged"]
  FilesAll -> git.getFiles
  FilesPrev -> git.getChangedFiles ["HEAD~1..HEAD"]

{----- RunMode -----}

data RunMode = Mode_Check | Mode_Fix | Mode_FixAdd
  deriving (Show, Eq)

allRunModes :: [RunMode]
allRunModes =
  [ Mode_Check
  , Mode_Fix
  , Mode_FixAdd
  ]

parseRunMode :: String -> Maybe RunMode
parseRunMode s = listToMaybe $ filter ((== s) . renderRunMode) allRunModes

renderRunMode :: RunMode -> String
renderRunMode = \case
  Mode_Check -> "check"
  Mode_Fix -> "fix"
  Mode_FixAdd -> "fix-add"
