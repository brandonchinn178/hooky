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

import Control.Concurrent (threadDelay)
import Control.Monad (forM_, unless, when)
import Data.Foldable qualified as Seq (toList)
import Data.IORef (atomicModifyIORef, newIORef, readIORef)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map qualified as Map
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Text.Lazy (LazyText)
import Data.Text.Lazy qualified as TextL
import Data.Text.Lazy.IO qualified as TextL
import Data.Time (diffUTCTime, getCurrentTime)
import Data.Time qualified as Time
import GHC.Records (HasField (..))
import Hooky.Config (
  Config,
  HookConfig,
  PassFilesMode (..),
  matchesGlobs,
 )
import Hooky.Config qualified as Config (Config (..))
import Hooky.Config qualified as HookConfig (HookConfig (..))
import Hooky.Config qualified as RepoConfig (RepoConfig (..))
import Hooky.Error (HookyError, abort)
import Hooky.Internal.Output (
  OutputFormat (..),
  outputLogLines,
  renderHookInProgressBody,
  renderHookInProgressHeader,
  renderHookReport,
  renderLogLines,
 )
import Hooky.Internal.Temp (hookyTmpDir)
import Hooky.Utils.Git (GitClient)
import Hooky.Utils.Process (renderShell, runStreamedProcess)
import Hooky.Utils.Term qualified as Term
import System.Console.Regions (
  ConsoleRegion,
  closeConsoleRegion,
  displayConsoleRegions,
  finishConsoleRegion,
  setConsoleRegion,
  withConsoleRegion,
 )
import System.Console.Regions qualified as Region (RegionLayout (..))
import System.Directory (findExecutable)
import System.Exit (ExitCode (..), exitFailure)
import System.FilePath ((</>))
import System.IO qualified as IO
import System.Process (getCurrentPid)
import UnliftIO.Async (pooledMapConcurrentlyN, withAsync)
import UnliftIO.Exception (
  bracket,
  catchAny,
  displayException,
  fromEitherM,
  try,
 )
import UnliftIO.Temporary (withSystemTempFile)

{----- runHooks -----}

data RunOptions = RunOptions
  { mode :: RunMode
  , fileTargets :: FileTargets
  , format :: OutputFormat
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
  (if options.stash then withStash git options.mode else id) $ do
    files <- resolveTargets git options.fileTargets
    let hooks = map (resolveHook config options files) config.repo.hooks
    let checkDiffs = initDiffChecker git options.mode
    results <- runHookCmds checkDiffs options.format maxHooks hooks
    printSummary results
    when (any ((== HookFailed) . snd) results) $ do
      exitFailure
 where
  maxParallelHooks = 5 -- TODO: make configurable - https://github.com/brandonchinn178/hooky/issues/3
  maxHooks =
    case options.mode of
      Mode_Check -> maxParallelHooks
      Mode_Fix
      Mode_FixAdd -> 1

withStash :: GitClient -> RunMode -> IO a -> IO a
withStash git mode = bracket' saveUntracked restoreUntracked . bracket' save restore
 where
  -- bracket where the result of 'before' is passed to 'after', but not the action
  bracket' before after = bracket before after . const

  -- Stash untracked files as well
  -- Treat untracked files as intent-to-add files so they're included in the
  -- stash, then unstage them afterwards
  saveUntracked = do
    untrackedFiles <- git.getFilesWith ["ls-files", "--others", "--exclude-standard"]
    if null untrackedFiles
      then pure Nothing
      else do
        git.exec $ ["add", "--intent-to-add", "--"] <> untrackedFiles
        pure $ Just untrackedFiles
  restoreUntracked = \case
    Nothing -> pure ()
    Just untrackedFiles -> do
      git.exec $ ["rm", "--cached", "--"] <> untrackedFiles

  save = do
    -- Get intent-to-add files
    itaFiles <- git.getFilesWith ["diff", "--name-only", "--diff-filter=A"]

    -- Get a phantom commit that includes staged files
    tree <- git.query ["write-tree"]

    -- Check if .hooky.kdl is to be staged
    configDiff <- git.query ["diff-index", Text.unpack tree, "--", ".hooky.kdl"]
    unless (Text.null configDiff) $ do
      abort ".hooky.kdl has changes, stage it first"

    diff <-
      fromEitherM . git.run $
        [ "diff-index"
        , "--ignore-submodules"
        , "--binary"
        , "--no-color"
        , "--no-ext-diff"
        , Text.unpack tree
        , "--"
        ]
    if Text.null diff
      then do
        pure Nothing
      else do
        pid <- getCurrentPid
        date <- Time.formatTime Time.defaultTimeLocale "%Y%m%d" <$> Time.getCurrentTime
        let stashFile = hookyTmpDir </> ("stash-" <> date <> "-" <> show pid)
        Text.writeFile stashFile diff
        outputLogLines $ "Stashed changes to: " <> TextL.pack stashFile
        unless (null itaFiles) $ do
          git.exec $ ["rm", "--force", "--"] <> itaFiles -- Remove intent-to-add files; persisted in the diff
        git.clearChanges
        pure $ Just (stashFile, itaFiles)
  restore = \case
    Nothing -> pure ()
    Just (stashFile, itaFiles) -> do
      let runGitApply =
            git.exec . concat $
              [ ["apply", "--whitespace=nowarn", stashFile]
              , case mode of
                  Mode_FixAdd -> ["--3way"]
                  _ -> ["--quiet"]
              ]
      try runGitApply >>= \case
        Right _ -> do
          outputLogLines $ "Restored changes from: " <> TextL.pack stashFile
        Left (_ :: HookyError) -> do
          case mode of
            Mode_Check
            Mode_Fix -> do
                outputLogLines . TextL.unlines $
                  [ "Stashed changes conflicted with hook modifications."
                  , "Run `hooky fix` manually and `git add` the desired changes."
                  , "Rolling back changes for now..."
                  ]
                git.clearChanges
                runGitApply
                pure () -- no need to explicitly fail, since hooks should have failed
            Mode_FixAdd -> do
              outputLogLines . TextL.unlines $
                [ "Stashed changes conflicted with fixed changes."
                , "Fix conflicts after git finishes the commit."
                ]
      -- Re-apply intent-to-add state
      unless (null itaFiles) $ do
        git.exec $ ["add", "--intent-to-add", "--"] <> itaFiles

runHookCmds :: DiffChecker -> OutputFormat -> Int -> [HookCmd] -> IO [(Text, HookResult)]
runHookCmds checkDiffs format maxHooks = displayConsoleRegions . pooledMapConcurrentlyN maxHooks run
 where
  maxOutputLines = 5 -- TODO: make configurable - https://github.com/brandonchinn178/hooky/issues/3
  run hook =
    withConsoleRegion Region.Linear $ \headerRegion ->
      withConsoleRegion (Region.InLine headerRegion) $ \outputRegion ->
        withAsync (renderHookHeaderAnimated headerRegion hook.name) $ \_ -> do
          hookOutput <- initHookOutput maxOutputLines $ \buf ->
            setConsoleRegion outputRegion $ renderHookInProgressBody buf
          hookOutput.log ["Running: " <> (TextL.fromStrict . renderShell . NonEmpty.toList) hook.args]
          (result, duration) <- withDuration $ runHook checkDiffs hookOutput hook
          when (shouldShowResult format result) $ do
            hookStdout <- hookOutput.getLines
            let output = if shouldShowStdout format result then hookStdout else []
            finishConsoleRegion headerRegion $ renderHookReport hook.name result.label output duration
            closeConsoleRegion outputRegion
          pure (hook.name, result)
  withDuration action = do
    start <- getCurrentTime
    a <- action
    end <- getCurrentTime
    pure (a, TextL.pack . show $ end `diffUTCTime` start)

renderHookHeaderAnimated :: ConsoleRegion -> Text -> IO ()
renderHookHeaderAnimated region name = go 0
 where
  animationFPS = 12 :: Int
  go t = do
    setConsoleRegion region $ renderHookInProgressHeader name t
    threadDelay (1000000 `div` animationFPS)
    go (t + 1)

{----- HookCmd -----}

data HookCmd = HookCmd
  { name :: Text
  , args :: NonEmpty Text
  , passFiles :: PassFilesMode
  , files :: [FilePath]
  , isSkip :: Bool
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
    , isSkip = hookConfig.name `Set.member` config.skippedHooks
    }
 where
  isIncluded fp = matchesGlobs (config.repo.files <> hookConfig.files) (Text.pack fp)

runHook :: DiffChecker -> HookOutput -> HookCmd -> IO HookResult
runHook checkDiffs hookOutput hook = do
  if null hook.files || hook.isSkip
    then pure HookSkipped
    else checkDiffs hookOutput $ do
      code <-
        case hook.passFiles of
          PassFiles_None -> run hook.args
          PassFiles_XArgs -> runXargs hook.args
          PassFiles_XArgsParallel -> runXargs $ "-P0" NonEmpty.<| hook.args
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
  run (cmd NonEmpty.:| args) =
    runProc cmd args $ \_ -> pure ()
  runXargs args =
    runProc "xargs" (["-0"] <> NonEmpty.toList args) $ \h ->
      forM_ hook.files $ \fp -> do
        IO.hPutStr h fp
        IO.hPutChar h '\0'
  runProc cmd args populateStdin =
    runStreamedProcess cmd args hookOutput.onLine populateStdin `catchAny` \e -> do
      -- See if it failed due to executable not being found
      let hookCmd = NonEmpty.head hook.args
      findExecutable (Text.unpack hookCmd) >>= \case
        Just _ -> pure ()
        Nothing -> abort $ "Executable does not exist: " <> hookCmd

      -- If the exception is unrecognized, just dump it to output
      abort . Text.unlines $
        [ "Failed to execute command: " <> renderShell (NonEmpty.toList hook.args)
        , Text.pack $ displayException e
        ]

{----- HookResult -----}

data HookResult = HookPassed | HookFailed | HookSkipped
  deriving (Show, Eq)

printSummary :: [(Text, HookResult)] -> IO ()
printSummary results
  | null results = pure ()
  | otherwise = do
      render HookPassed ("passed " <> Term.green "✔")
      render HookFailed ("failed " <> Term.red "✘")
      render HookSkipped ("skipped " <> Term.yellow "≫")
 where
  render status label = do
    let count = length $ filter ((== status) . snd) results
    when (count > 0) $ do
      TextL.putStrLn . TextL.unwords $
        [ TextL.pack $ show count
        , if count == 1 then "hook" else "hooks"
        , label
        ]

instance HasField "label" HookResult LazyText where
  getField = \case
    HookFailed -> Term.redBG "FAIL"
    HookPassed -> Term.greenBG "PASS"
    HookSkipped -> Term.yellowBG "SKIP"

shouldShowResult :: OutputFormat -> HookResult -> Bool
shouldShowResult format = \case
  HookFailed -> format >= Format_Minimal
  HookPassed -> format >= Format_Full
  HookSkipped -> format >= Format_Full

shouldShowStdout :: OutputFormat -> HookResult -> Bool
shouldShowStdout format = \case
  HookFailed -> format >= Format_Minimal
  HookPassed -> format >= Format_Verbose
  HookSkipped -> format >= Format_Verbose

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
  deriving (Show, Eq, Enum, Bounded)

allRunModes :: [RunMode]
allRunModes = [minBound .. maxBound]

parseRunMode :: String -> Maybe RunMode
parseRunMode = flip Map.lookup x
 where
  x = Map.fromList [(renderRunMode m, m) | m <- allRunModes]

renderRunMode :: RunMode -> String
renderRunMode = \case
  Mode_Check -> "check"
  Mode_Fix -> "fix"
  Mode_FixAdd -> "fix-add"

{----- HookOutput -----}

data HookOutput = HookOutput
  { onLine :: Text -> IO ()
  , log :: [LazyText] -> IO ()
  , getLines :: IO [LazyText]
  }

initHookOutput :: Int -> ([LazyText] -> IO ()) -> IO HookOutput
initHookOutput maxOutputLines renderBuf = do
  outputRef <- newIORef ([], Seq.empty)
  let onLine line = do
        progressLines <-
          atomicModifyIORef outputRef $ \(prev, buf) ->
            let buf' = (if Seq.length buf == maxOutputLines then Seq.drop 1 buf else buf) Seq.|> line
             in ((line : prev, buf'), Seq.toList buf')
        renderBuf progressLines
  pure
    HookOutput
      { onLine = onLine . TextL.fromStrict
      , log = mapM_ onLine . renderLogLines
      , getLines = reverse . fst <$> readIORef outputRef
      }

{----- DiffChecker -----}

type DiffChecker = HookOutput -> IO HookResult -> IO HookResult

initDiffChecker :: GitClient -> RunMode -> DiffChecker
initDiffChecker git mode = \hookOutput action -> do
  before <- git.getDiff
  result <- action
  after <- git.getDiff
  case mode of
    _ | before == after -> do
      pure result
    Mode_Check -> do
      hookOutput.log
        [ "Files were unexpectedly modified."
        , "Running hooks with --mode=check should NOT modify files, since hooks are run in"
        , "parallel. Fix the commands in the hooky config to only modify files with"
        , "`fix_args`."
        ]
      pure HookFailed
    Mode_Fix -> do
      hookOutput.log
        [ "Files were modified."
        , "Run `git add` to stage the changes."
        ]
      pure HookFailed
    Mode_FixAdd -> do
      modifiedFiles <- git.getChangedFiles []
      git.exec ("add" : modifiedFiles)
      pure result
