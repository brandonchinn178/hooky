{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}

import Control.Monad (forM, guard, unless)
import Data.Proxy (Proxy (..))
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
import System.Directory (doesFileExist, makeAbsolute)
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
  | CommandRun
  | CommandFix
  | CommandLint
      { files :: [FilePath]
      , autofix :: Bool
      }

cliOptions :: Opt.ParserInfo CLIOptions
cliOptions =
  Opt.info (Opt.helper <*> parseOptions) . mconcat $
    [ Opt.fullDesc
    , Opt.header "Hooky: A minimal git hooks manager"
    ]
 where
  parseOptions = do
    command <-
      Opt.hsubparser . mconcat $
        [ Opt.command "install" (Opt.info parseInstall $ Opt.progDesc "Install hooky as the git pre-commit hook")
        , Opt.command "run" (Opt.info parseRun $ Opt.progDesc "Run hooks")
        , Opt.command "fix" (Opt.info parseFix $ Opt.progDesc "Run hooks with autofixing enabled")
        , Opt.command "lint" (Opt.info parseLint $ Opt.progDesc "Run builtin hooky lint rules")
        ]

    configFile <-
      Opt.optional . Opt.strOption . mconcat $
        [ Opt.long "config"
        , Opt.short 'c'
        , Opt.help "Path to config file (default: .hooky.kdl)"
        ]

    pure CLIOptions{..}

  parseInstall = do
    pure CommandInstall

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
  cli <- Opt.execParser cliOptions

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
    CommandInstall -> do
      -- exe <- getExecutablePath >>= parseAbsFile
      -- let args = ["--config", Text.pack (toFilePath configFile)]
      -- doInstall repo exe args
      error "TODO: install"
    CommandRun -> do
      -- config <- loadConfig configFile
      -- success <-
      --   doRun repo config $
      --     RunOptions
      --       { showStdoutOnSuccess = cliLogLevel >= Verbose
      --       }
      -- unless success exitFailure
      abort "TODO: run"
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
