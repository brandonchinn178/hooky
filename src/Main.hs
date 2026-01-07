{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}

import Control.Applicative ((<|>))
import Control.Monad (forM, guard, unless, when)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Typeable (typeOf, typeRep)
import Hooky.Config (Config (..), parseConfig)
import Hooky.Error (abort)
import Hooky.Lint (
  LintRunConfig (..),
  lintReportSuccess,
  renderLintReport,
  runLintRules,
 )
import Hooky.Utils.Git (GitClient (..), initGitClient)
import Options.Applicative qualified as Opt
import System.Directory (
  doesFileExist,
  getPermissions,
  makeAbsolute,
  renameFile,
  setPermissions,
 )
import System.Directory qualified as Permissions (Permissions (..))
import System.Environment (getExecutablePath)
import System.Exit (ExitCode, exitFailure)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import UnliftIO.Exception (Exception (..), SomeException (..), handleJust)

{----- CLI Options -----}

data CLIOptions = CLIOptions
  { command :: CLICommand
  , configFile :: Maybe FilePath
  }

data CLICommand
  = CommandInstall
      { mode :: RunGitMode
      }
  | CommandRunGit
      { mode :: RunGitMode
      }
  | CommandRun
  | CommandFix
  | CommandLint
      { files :: [FilePath]
      , autofix :: Bool
      }

loadCLIOptions :: IO CLIOptions
loadCLIOptions =
  Opt.customExecParser (Opt.prefs $ mconcat prefs) $
    Opt.info (Opt.helper <*> parseOptions) . mconcat $
      [ Opt.fullDesc
      , Opt.header "Hooky: A minimal git hooks manager"
      ]
 where
  prefs =
    [ Opt.subparserInline
    ]

  parseOptions = do
    command <- parseInternalCommand <|> parseCommand

    configFile <-
      Opt.optional . Opt.strOption . mconcat $
        [ Opt.long "config"
        , Opt.short 'c'
        , Opt.help "Path to config file (default: .hooky.kdl)"
        ]

    pure CLIOptions{..}

  parseInternalCommand =
    Opt.subparser . mconcat $
      [ Opt.internal
      , Opt.command "__git" (Opt.info parseRunGit $ Opt.progDesc "Run as a git hook")
      ]

  parseCommand =
    Opt.hsubparser . mconcat $
      [ Opt.command "install" (Opt.info parseInstall $ Opt.progDesc "Install hooky as the git pre-commit hook")
      , Opt.command "run" (Opt.info parseRun $ Opt.progDesc "Run hooks")
      , Opt.command "fix" (Opt.info parseFix $ Opt.progDesc "Run hooks with autofixing enabled")
      , Opt.command "lint" (Opt.info parseLint $ Opt.progDesc "Run builtin hooky lint rules")
      ]

  parseInstall = do
    mode <- parseRunGitMode
    pure CommandInstall{..}

  parseRunGit = do
    mode <- parseRunGitMode
    pure CommandRunGit{..}

  parseRunGitMode = do
    let modes = uncommas $ map renderRunGitMode allRunGitModes
        uncommas = Text.unpack . Text.intercalate ", " . map Text.pack
    fmap (fromMaybe RunGit_Check) . Opt.optional $
      Opt.option (Opt.maybeReader readRunGitMode) . mconcat $
        [ Opt.long "mode"
        , Opt.help $ "Mode to run hooky in during git hooks. One of: " <> modes
        ]

  parseRun = do
    pure CommandRun

  parseFix = do
    pure CommandFix

  parseLint = do
    files <-
      Opt.some . Opt.argument Opt.str . mconcat $
        [ Opt.metavar "FILES"
        ]
    autofix <-
      Opt.switch . mconcat $
        [ Opt.long "fix"
        , Opt.help "Whether to fix issues"
        ]
    pure CommandLint{..}

{----- Entrypoint -----}

main :: IO ()
main = handleErrors $ do
  cli <- loadCLIOptions

  git <- initGitClient
  configFile <-
    case cli.configFile of
      Nothing -> pure $ git.repo </> ".hooky.kdl"
      Just fp -> makeAbsolute fp

  configFileExists <- doesFileExist configFile
  unless configFileExists $ do
    abort $ "Config file doesn't exist: " <> Text.pack configFile

  config <- either abort pure . parseConfig =<< Text.readFile configFile

  case cli.command of
    CommandInstall{..} ->
      cmdInstall git $
        [ "--config"
        , Text.pack configFile
        , "--mode"
        , Text.pack $ renderRunGitMode mode
        ]
    CommandRun -> do
      -- config <- loadConfig configFile
      -- success <-
      --   doRun repo config $
      --     RunOptions
      --       { showStdoutOnSuccess = cliLogLevel >= Verbose
      --       }
      -- unless success exitFailure
      abort "TODO: run"
    CommandRunGit{} -> do
      -- TODO:
      --   if
      -- config <- loadConfig configFile
      -- success <-
      --   doRun repo config $
      --     RunOptions
      --       { showStdoutOnSuccess = cliLogLevel >= Verbose
      --       }
      -- unless success exitFailure
      putStrLn "TODO: run git"
    CommandFix -> do
      abort "TODO: fix"
    command@CommandLint{} ->
      cmdLint
        git
        LintRunConfig
          { autofix = command.autofix
          , rules = config.lintRules
          }
        command.files

handleErrors :: IO a -> IO a
handleErrors = handleJust shouldHandle $ \(SomeException e) -> do
  hPutStrLn stderr $ "hooky: " <> strip (displayException e)
  exitFailure
 where
  strip = Text.unpack . Text.strip . Text.pack
  shouldHandle (SomeException e) = do
    guard $ typeOf e `notElem` ignoredErrors
    Just (SomeException e)
  ignoredErrors =
    [ typeRep (Proxy @ExitCode)
    ]

cmdInstall :: GitClient -> [Text] -> IO ()
cmdInstall git args = do
  precommitHookFile <- Text.unpack <$> git.getPath "hooks/pre-commit"
  precommitHookFileExists <- doesFileExist precommitHookFile
  -- TODO: Don't back up if reinstalling hooky
  when precommitHookFileExists $ do
    let backup = precommitHookFile <> ".bak"
    renameFile precommitHookFile backup
    let border = Text.replicate 50 "*"
    Text.putStrLn border
    Text.putStrLn "Found previously installed pre-commit hooks."
    Text.putStrLn $ "Backed up to: " <> Text.pack backup
    Text.putStrLn border

  exe <- getExecutablePath
  Text.writeFile precommitHookFile . Text.unlines $
    [ Text.unwords $ ["exec", Text.pack exe, "__git"] <> args
    ]
  makeExecutable precommitHookFile

  Text.putStrLn $ "Hooky installed at: " <> Text.pack precommitHookFile
 where
  makeExecutable fp = do
    p <- getPermissions fp
    setPermissions fp p{Permissions.executable = True}

data RunGitMode = RunGit_Check | RunGit_Fix | RunGit_FixAdd
  deriving (Show, Eq)

allRunGitModes :: [RunGitMode]
allRunGitModes =
  [ RunGit_Check
  , RunGit_Fix
  , RunGit_FixAdd
  ]

readRunGitMode :: String -> Maybe RunGitMode
readRunGitMode s = listToMaybe $ filter ((== s) . renderRunGitMode) allRunGitModes

renderRunGitMode :: RunGitMode -> String
renderRunGitMode = \case
  RunGit_Check -> "check"
  RunGit_Fix -> "fix"
  RunGit_FixAdd -> "fix-add"

cmdLint :: GitClient -> LintRunConfig -> [FilePath] -> IO ()
cmdLint git lintConfig files0 = do
  files <- fmap concat . forM files0 $ \file ->
    case file of
      '@' : f -> map Text.unpack . Text.lines <$> Text.readFile f
      f -> pure [f]

  report <- runLintRules git lintConfig files
  Text.putStrLn $ renderLintReport report
  unless (lintReportSuccess report) $ do
    exitFailure
