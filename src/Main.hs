{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OrPatterns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}

import Control.Applicative ((<|>))
import Control.Monad (guard, unless, when)
import Data.Choice (
  Choice,
  pattern Should,
  pattern Shouldn't,
 )
import Data.Choice qualified as Choice
import Data.Maybe (fromMaybe, listToMaybe)
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
  { run :: CLICommandAction
  , configFile :: Maybe FilePath
  }

type CLICommandAction =
  GitClient ->
  (FilePath, Config) ->
  IO ()

data CLICommandDef
  = forall args.
  (IsCLIArgs args) =>
  CLICommandDef
  { name :: String
  , description :: String
  , argsType :: Proxy args
  }

class IsCLIArgs args where
  cliArgsParse :: Opt.Parser args

  cliArgsRun :: args -> CLICommandAction

  cliArgsFiles :: Maybe (args -> [FilePath], [FilePath] -> args -> args)
  cliArgsFiles = Nothing

loadCLIOptions :: IO CLIOptions
loadCLIOptions =
  Opt.customExecParser prefs $
    Opt.info (Opt.helper <*> parseOptions) . mconcat $
      [ Opt.fullDesc
      , Opt.header "Hooky: A minimal git hooks manager"
      ]
 where
  prefs =
    Opt.prefs . mconcat $
      [ Opt.subparserInline
      ]
  parseOptions = do
    run <- parseInternalCommand <|> parseCommand
    configFile <-
      Opt.optional . Opt.strOption . mconcat $
        [ Opt.long "config"
        , Opt.short 'c'
        , Opt.help "Path to config file (default: .hooky.kdl)"
        ]
    pure CLIOptions{..}

  parseInternalCommand =
    Opt.subparser . (Opt.internal <>) . mconcat . map mkCommand $
      [ cmdRunGit
      ]
  parseCommand =
    Opt.hsubparser . mconcat . map mkCommand $
      [ cmdInstall
      , cmdRunGit
      , cmdRun
      , cmdFix
      , cmdLint
      ]

  mkCommand CLICommandDef{..} =
    Opt.command name $
      Opt.info (mkAction argsType <$> cliArgsParse) $
        Opt.progDesc description
  mkAction :: (IsCLIArgs args) => Proxy args -> args -> CLICommandAction
  mkAction _ args git config = do
    args' <- resolveFiles args
    cliArgsRun args' git config

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

  cli.run git (configFile, config)

-- | Resolve any `@files` arguments specified as arguments
resolveFiles :: (IsCLIArgs args) => args -> IO args
resolveFiles args =
  case cliArgsFiles of
    Nothing -> pure args
    Just (getFiles, setFiles) -> do
      files' <- resolve (getFiles args)
      pure $ setFiles files' args
 where
  resolve = fmap concat . mapM resolveFile
  resolveFile = \case
    '@' : file -> map Text.unpack . Text.lines <$> Text.readFile file
    file -> pure [file]

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

{----- hooky install -----}

cmdInstall :: CLICommandDef
cmdInstall =
  CLICommandDef
    { name = "install"
    , description = "Install hooky as the git pre-commit hook"
    , argsType = Proxy @Args_Install
    }

data Args_Install = Args_Install
  { mode :: RunMode
  }

instance IsCLIArgs Args_Install where
  cliArgsParse = do
    mode <- parseRunMode
    pure Args_Install{..}
  cliArgsRun args git (configFile, _) = do
    hookFile <- Text.unpack <$> git.getPath "hooks/pre-commit"
    backupOldHookFile hookFile

    exe <- getExecutablePath
    Text.writeFile hookFile . Text.unlines $
      [ Text.unwords $
          [ "exec"
          , quote . Text.pack $ exe
          , "__git"
          , "--config"
          , quote . Text.pack $ configFile
          , "--mode"
          , quote . Text.pack $ renderRunMode args.mode
          ]
      ]
    makeExecutable hookFile

    Text.putStrLn $ "Hooky installed at: " <> Text.pack hookFile
   where
    quote s = "'" <> s <> "'"
    makeExecutable fp = do
      p <- getPermissions fp
      setPermissions fp p{Permissions.executable = True}

    backupOldHookFile fp = do
      exists <- doesFileExist fp
      -- TODO: Don't back up if reinstalling hooky
      when exists $ do
        let backup = fp <> ".bak"
        renameFile fp backup
        let border = Text.replicate 50 "*"
        Text.putStrLn border
        Text.putStrLn "Found previously installed pre-commit hooks."
        Text.putStrLn $ "Backed up to: " <> Text.pack backup
        Text.putStrLn border

{----- hooky __git -----}

cmdRunGit :: CLICommandDef
cmdRunGit =
  CLICommandDef
    { name = "__git"
    , description = "Run as a git hook"
    , argsType = Proxy @Args_RunGit
    }

data Args_RunGit = Args_RunGit
  { mode :: RunMode
  }

instance IsCLIArgs Args_RunGit where
  cliArgsParse = do
    mode <- parseRunMode
    pure Args_RunGit{..}
  cliArgsRun args git (_, config) = do
    files <- pure []
    runHooks git config shouldFix files
    case args.mode of
      Mode_FixAdd -> do
        pure () -- TODO: add modified files
      _ -> pure ()
   where
    shouldFix =
      case args.mode of
        Mode_Check -> Shouldn't #fix
        Mode_Fix
        Mode_FixAdd -> Should #fix

{----- hooky run ------}

cmdRun :: CLICommandDef
cmdRun =
  CLICommandDef
    { name = "run"
    , description = "Run hooks"
    , argsType = Proxy @Args_Run
    }

data Args_Run = Args_Run
  { files :: [FilePath]
  }

instance IsCLIArgs Args_Run where
  cliArgsParse = do
    files <- pure []
    pure Args_Run{..}

  cliArgsRun args git (_, config) = do
    runHooks git config (Shouldn't #fix) args.files

  cliArgsFiles = Just ((.files), \files' Args_Run{} -> Args_Run{files = files', ..})

runHooks :: GitClient -> Config -> Choice "fix" -> [FilePath] -> IO ()
runHooks _ _ shouldFix _ = do
  -- config <- loadConfig configFile
  -- success <-
  --   doRun repo config $
  --     RunOptions
  --       { showStdoutOnSuccess = cliLogLevel >= Verbose
  --       }
  -- unless success exitFailure
  if Choice.isTrue shouldFix
    then do
      abort $ "TODO: fix"
    else do
      putStrLn $ "TODO: check"

{----- hooky fix ------}

cmdFix :: CLICommandDef
cmdFix =
  CLICommandDef
    { name = "fix"
    , description = "Run hooks with autofixing enabled"
    , argsType = Proxy @Args_Fix
    }

data Args_Fix = Args_Fix
  { files :: [FilePath]
  }

instance IsCLIArgs Args_Fix where
  cliArgsParse = do
    files <- pure []
    pure Args_Fix{..}

  cliArgsRun args git (_, config) = do
    runHooks git config (Should #fix) args.files

  cliArgsFiles = Just ((.files), \files' Args_Fix{} -> Args_Fix{files = files', ..})

{----- hooky lint ------}

cmdLint :: CLICommandDef
cmdLint =
  CLICommandDef
    { name = "lint"
    , description = "Run builtin hooky lint rules"
    , argsType = Proxy @Args_Lint
    }

data Args_Lint = Args_Lint
  { files :: [FilePath]
  , autofix :: Bool
  }

instance IsCLIArgs Args_Lint where
  cliArgsParse = do
    files <-
      Opt.some . Opt.argument Opt.str . mconcat $
        [ Opt.metavar "FILES"
        ]
    autofix <-
      Opt.switch . mconcat $
        [ Opt.long "fix"
        , Opt.help "Whether to fix issues"
        ]
    pure Args_Lint{..}

  cliArgsRun args git (_, config) = do
    report <- runLintRules git lintConfig args.files
    Text.putStrLn $ renderLintReport report
    unless (lintReportSuccess report) $ do
      exitFailure
   where
    lintConfig =
      LintRunConfig
        { autofix = args.autofix
        , rules = config.lintRules
        }

  cliArgsFiles = Just ((.files), \files' Args_Lint{..} -> Args_Lint{files = files', ..})

{----- RunMode -----}

data RunMode = Mode_Check | Mode_Fix | Mode_FixAdd
  deriving (Show, Eq)

allRunModes :: [RunMode]
allRunModes =
  [ Mode_Check
  , Mode_Fix
  , Mode_FixAdd
  ]

parseRunMode :: Opt.Parser RunMode
parseRunMode = do
  let modes = uncommas $ map renderRunMode allRunModes
      uncommas = Text.unpack . Text.intercalate ", " . map Text.pack
  fmap (fromMaybe Mode_Check) . Opt.optional $
    Opt.option (Opt.maybeReader parse) . mconcat $
      [ Opt.long "mode"
      , Opt.help $ "Mode to run hooky in during git hooks. One of: " <> modes
      ]
 where
  parse s = listToMaybe $ filter ((== s) . renderRunMode) allRunModes

renderRunMode :: RunMode -> String
renderRunMode = \case
  Mode_Check -> "check"
  Mode_Fix -> "fix"
  Mode_FixAdd -> "fix-add"
