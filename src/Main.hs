{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Monad (forM, unless)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Hooky.Config (Config (..), parseConfig)
import Hooky.Lint (
  LintRunConfig (..),
  renderLintReport,
  runLintRules,
 )
import Options.Applicative qualified as Opt
-- import System.Environment (getExecutablePath)
import System.Directory (doesFileExist, makeAbsolute)
import System.Exit (ExitCode (..), exitFailure)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import System.Process (readProcessWithExitCode)

{----- CLI Options -----}

data CLIOptions = CLIOptions
  { command :: CLICommand
  , configFilePath :: Maybe FilePath
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

      configFilePath <-
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
main = do
  CLIOptions{..} <- Opt.execParser cliOptions

  repo <- readProcessWithExitCode "git" ["rev-parse", "--show-toplevel"] "" >>= \case
    (ExitFailure _, _, _) -> abort "hooky: not currently in a git repository"
    (ExitSuccess, stdout, _) -> pure $ (Text.unpack . Text.strip . Text.pack) stdout

  configFile <-
    case configFilePath of
      Nothing -> pure $ repo </> ".hooky.kdl"
      Just fp -> makeAbsolute fp

  configFileExists <- doesFileExist configFile
  unless configFileExists $
    abort $ "Config file doesn't exist: " <> configFile

  config <- either (abort . Text.unpack) pure . parseConfig =<< Text.readFile configFile

  case command of
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
      error "TODO: run"
    CommandFix -> do
      error "TODO: fix"
    CommandLint{..} ->
      cmdLint
        LintRunConfig
          { repo = repo
          , autofix = autofix
          , rules = let Config{lintRules} = config in lintRules
          }
        files

cmdLint :: LintRunConfig -> [FilePath] -> IO ()
cmdLint lintConfig files0 = do
  files <- fmap concat . forM files0 $ \file ->
    case file of
      '@' : f -> map Text.unpack . Text.lines <$> Text.readFile f
      f -> pure [f]

  report <- runLintRules lintConfig files
  Text.putStrLn $ renderLintReport report

abort :: String -> IO a
abort s = hPutStrLn stderr s >> exitFailure
