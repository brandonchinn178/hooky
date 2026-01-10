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
import Control.Monad (forM_, when)
import Data.Char (isSpace)
import Data.Foldable qualified as Seq (toList)
import Data.IORef (atomicModifyIORef, newIORef, readIORef)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (listToMaybe)
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as Lazy
import Data.Text.Lazy qualified as TextL
import Data.Text.Lazy.IO qualified as TextL
import GHC.Records (HasField (..))
import Hooky.Config (Config, HookConfig, PassFilesMode (..), matchesGlobs)
import Hooky.Config qualified as Config (Config (..))
import Hooky.Config qualified as HookConfig (HookConfig (..))
import Hooky.Utils.Git (GitClient)
import Hooky.Utils.Process (runStreamedProcess)
import Hooky.Utils.Term qualified as Term
import System.Console.Regions (
  ConsoleRegion,
  displayConsoleRegions,
  finishConsoleRegion,
  setConsoleRegion,
  withConsoleRegion,
 )
import System.Console.Regions qualified as Region (RegionLayout (..))
import System.Exit (ExitCode (..), exitFailure)
import System.IO qualified as IO
import UnliftIO.Async (pooledMapConcurrentlyN, withAsync)
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
    results <- runHookCmds hooks options.mode
    printSummary results
    when (any ((== HookFailed) . snd) results) $ do
      exitFailure
 where
  withStash = id -- FIXME

runHookCmds :: [HookCmd] -> RunMode -> IO [(Text, HookResult)]
runHookCmds hooks mode =
  displayConsoleRegions $ do
    result <- pooledMapConcurrentlyN maxHooks run hooks
    case mode of
      Mode_Check
      Mode_Fix -> checkModified
      Mode_FixAdd -> stageModified
    pure result
 where
  -- TODO: make configurable
  maxParallelHooks = 5
  maxOutputLines = 5

  maxHooks =
    case mode of
      Mode_Check -> maxParallelHooks
      Mode_Fix
      Mode_FixAdd -> 1

  checkModified = pure () -- FIXME: error if files modified
  stageModified = pure () -- FIXME: stage modified files
  run hook =
    withConsoleRegion Region.Linear $ \headerRegion ->
      withConsoleRegion (Region.InLine headerRegion) $ \outputRegion ->
        withAsync (renderHeader headerRegion hook.name) $ \_ -> do
          outputRef <- newIORef ([], Seq.empty)
          let onOutputLine line = do
                progressLines <-
                  atomicModifyIORef outputRef $ \(prev, buf) ->
                    let buf' = (if Seq.length buf == maxOutputLines then Seq.drop 1 buf else buf) Seq.|> line
                     in ((line : prev, buf'), Seq.toList buf')
                setConsoleRegion outputRegion $ renderSectionBody progressLines

          onOutputLine $ "═══▶ Running: " <> (renderShell . NonEmpty.toList) hook.args
          result <- runHook hook onOutputLine
          case result of
            HookFailed -> do
              let header = renderHookStatus hook.name (Term.redBG "FAIL")
              output <- renderSectionBody . reverse . fst <$> readIORef outputRef
              finishConsoleRegion headerRegion header
              finishConsoleRegion outputRegion $ TextL.stripEnd output
            _ -> pure ()
          pure (hook.name, result)

  renderShell args =
    Text.intercalate " " $
      [ if Text.any isSpace s then "'" <> s <> "'" else s
      | s <- args
      ]

renderHookStatus :: Text -> Lazy.Text -> Lazy.Text
renderHookStatus name status =
  TextL.concat
    [ "╭─── "
    , status
    , " "
    , Term.bold $ TextL.fromStrict name
    , " "
    ]

renderSectionBody :: [Text] -> Lazy.Text
renderSectionBody = TextL.unlines . map (("│ " <>) . TextL.fromStrict)

renderHeader :: ConsoleRegion -> Text -> IO ()
renderHeader region name = go 0
 where
  totalWidth = 6 :: Int
  barWidth = 3 :: Int
  animationFPS = 12 :: Int

  start = renderHookStatus name (Term.yellowBG "RUNNING")

  go t = do
    setConsoleRegion region . TextL.concat $
      [ start
      , "["
      , TextL.pack $ map (getBarChar t) [0 .. totalWidth - 1]
      , "]\n"
      ]
    threadDelay (1000000 `div` animationFPS)
    go (t + 1)

  getBarChar t i =
    if any (== i) . map (`mod` totalWidth) . map (t +) $ [0 .. barWidth - 1]
      then '='
      else ' '

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

runHook :: HookCmd -> (Text -> IO ()) -> IO HookResult
runHook hook onOutput = do
  if null hook.files
    then pure HookSkipped
    else do
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
    runStreamedProcess cmd args onOutput $ \_ -> pure ()
  runXargs args =
    runStreamedProcess "xargs" (["-0"] <> NonEmpty.toList args) onOutput $ \h ->
      forM_ hook.files $ \fp -> do
        IO.hPutStr h fp
        IO.hPutChar h '\0'

{----- HookResult -----}

data HookResult = HookPassed | HookFailed | HookSkipped
  deriving (Show, Eq)

printSummary :: [(Text, HookResult)] -> IO ()
printSummary results
  | null results = pure ()
  | otherwise = do
      TextL.putStrLn ""
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
