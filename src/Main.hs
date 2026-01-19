{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OrPatterns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}

import Control.Applicative ((<**>), (<|>))
import Control.Monad (guard, unless, when)
import Data.Coerce (coerce)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Proxy (Proxy (..))
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Text.Lazy qualified as TextL
import Data.Text.Lazy.IO qualified as TextL
import Data.Typeable (typeOf, typeRep)
import Data.Version (showVersion)
import Hooky.Config (Config (..), loadConfig)
import Hooky.Error (abort, abortImpure)
import Hooky.Internal.Output (
  OutputFormat (..),
  allOutputFormats,
  outputLogLines,
  parseOutputFormat,
  renderOutputFormat,
 )
import Hooky.Lint (
  LintOptions (..),
  lintReportSuccess,
  renderLintReport,
  runLintRules,
 )
import Hooky.Run (
  FileTargets (..),
  RunMode (..),
  RunOptions (..),
  allRunModes,
  parseRunMode,
  renderRunMode,
  runHooks,
 )
import Hooky.Utils.Git (GitClient (..), initGitClient)
import Hooky.Utils.Term qualified as Term
import Options.Applicative qualified as Opt
import Options.Applicative.Types qualified as Opt.Internal
import Paths_hooky qualified
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
import System.IO (stderr)
import UnliftIO.Exception (Exception (..), SomeException (..), handleJust)

{----- CLI Options -----}

data CLIOptions = CLIOptions
  { run :: CLICommandAction
  , configFile :: Maybe FilePath
  }

type CLICommandAction = GitClient -> Config -> IO ()

data CLICommandDef
  = forall cmd.
  (IsCLICommand cmd) =>
  CLICommandDef
  { name :: String
  , description :: String
  , cmdType :: Proxy cmd
  }

class IsCLICommand cmd where
  cliCommandParse :: Opt.Parser cmd

  cliCommandRun :: cmd -> CLICommandAction

  cliCommandFiles :: cmd -> Maybe ([FilePath], [FilePath] -> cmd)
  cliCommandFiles _ = Nothing

mkAction :: (IsCLICommand cmd) => Proxy cmd -> cmd -> CLICommandAction
mkAction _ cmd git config = do
  cmd' <- resolveFiles cmd
  cliCommandRun cmd' git config

loadCLIOptions :: IO CLIOptions
loadCLIOptions =
  Opt.customExecParser prefs $
    Opt.info (parseOptions <**> Opt.helper <**> version) . mconcat $
      [ Opt.fullDesc
      , Opt.header "Hooky: A minimal git hooks manager"
      ]
 where
  prefs =
    Opt.prefs . mconcat $
      [ Opt.subparserInline
      ]
  version =
    Opt.infoOption (showVersion Paths_hooky.version) . mconcat $
      [ Opt.long "version"
      , Opt.help "Print version of hooky"
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
      Opt.info (mkAction cmdType <$> cliCommandParse) $
        Opt.progDesc description

{----- Entrypoint -----}

main :: IO ()
main = handleErrors $ do
  cli <- loadCLIOptions

  git <- initGitClient

  repoConfigPath <-
    case cli.configFile of
      Nothing -> pure $ git.repo </> ".hooky.kdl"
      Just fp -> makeAbsolute fp
  config <- loadConfig repoConfigPath

  cli.run git config

-- | Resolve files specified as arguments.
--
-- Expands `@file` arguments to files specified in the given file.
resolveFiles :: (IsCLICommand cmd) => cmd -> IO cmd
resolveFiles cmd =
  case cliCommandFiles cmd of
    Nothing -> pure cmd
    Just (files, setFiles) -> do
      files' <- resolve files
      pure $ setFiles files'
 where
  resolve = fmap concat . mapM resolveFile
  resolveFile = \case
    '@' : file -> map Text.unpack . Text.lines <$> Text.readFile file
    file -> do
      exist <- doesFileExist file
      unless exist $ do
        abort $ "File does not exist: " <> Text.pack file
      pure [file]

handleErrors :: IO a -> IO a
handleErrors = handleJust shouldHandle $ \(SomeException e) -> do
  TextL.hPutStrLn stderr . Term.red $ (TextL.strip . TextL.pack . displayException) e
  exitFailure
 where
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
    , cmdType = Proxy @Cmd_Install
    }

data Cmd_Install = Cmd_Install
  { runGitOpts :: Cmd_RunGit
  }

instance IsCLICommand Cmd_Install where
  cliCommandParse = do
    runGitOpts <- cliCommandParse
    pure Cmd_Install{..}
  cliCommandRun cmd git config = do
    hookFile <- Text.unpack <$> git.getPath "hooks/pre-commit"
    backupOldHookFile hookFile

    exe <- getExecutablePath
    Text.writeFile hookFile . Text.unlines $
      [ Text.unwords $
          [ "exec"
          , quote . Text.pack $ exe
          , "__git"
          , "--config"
          , quote . Text.pack $ config.repoConfigPath
          , "--mode"
          , quote . Text.pack $ renderRunMode cmd.runGitOpts.mode
          , "--format"
          , quote . Text.pack $ renderOutputFormat cmd.runGitOpts.format
          ]
      ]
    makeExecutable hookFile

    TextL.putStrLn . Term.green $ "🚀 Hooky installed at: " <> TextL.pack hookFile
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
        outputLogLines . TextL.unlines $
          [ "Found previously installed pre-commit hooks."
          , "Backed up to: " <> TextL.pack backup
          ]

{----- hooky __git -----}

cmdRunGit :: CLICommandDef
cmdRunGit =
  CLICommandDef
    { name = "__git"
    , description = "Run as a git hook"
    , cmdType = Proxy @Cmd_RunGit
    }

data Cmd_RunGit = Cmd_RunGit
  { mode :: RunMode
  , format :: OutputFormat
  }

instance IsCLICommand Cmd_RunGit where
  cliCommandParse = do
    mode <- parseRunModeCLI
    format <- parseFormatCLI
    pure Cmd_RunGit{..}
  cliCommandRun cmd git config = do
    runHooks git config $
      RunOptions
        { mode = cmd.mode
        , fileTargets = FilesStaged
        , format = cmd.format
        , stash = True
        }

{----- hooky run ------}

cmdRun :: CLICommandDef
cmdRun =
  CLICommandDef
    { name = "run"
    , description = "Run hooks"
    , cmdType = Proxy @Cmd_Run
    }

data Cmd_Run = Cmd_Run
  { mode :: RunMode
  , fileTargets :: FileTargets
  , stash :: Bool
  , format :: OutputFormat
  }

instance IsCLICommand Cmd_Run where
  cliCommandParse = do
    mFileTargets <-
      cliOneOfOptional $
        [ FilesGiven <$> parseFilesCLI
        , Opt.flag' FilesModified . mconcat $
            [ Opt.long "modified"
            , Opt.short 'm'
            , Opt.help "Run on modified files"
            ]
        , Opt.flag' FilesStaged . mconcat $
            [ Opt.long "staged"
            , Opt.short 's'
            , Opt.help "Run on staged files"
            ]
        , Opt.flag' FilesAll . mconcat $
            [ Opt.long "all"
            , Opt.short 'a'
            , Opt.help "Run on all files"
            ]
        , Opt.flag' FilesPrev . mconcat $
            [ Opt.long "prev"
            , Opt.short '1'
            , Opt.help "Run on files modified in the previous commit"
            ]
        ]
    stashFlag <-
      Opt.switch . mconcat $
        [ Opt.long "stash"
        , Opt.help "Stash unstaged changes before running hooks"
        ]
    format <- parseFormatCLI

    pure $
      let (fileTargets, stash) =
            case mFileTargets of
              Nothing -> (FilesStaged, True)
              Just ft -> (ft, stashFlag)
       in Cmd_Run{mode = Mode_Check, ..}

  cliCommandRun cmd git config = do
    runHooks git config $
      RunOptions
        { mode = cmd.mode
        , fileTargets = cmd.fileTargets
        , format = cmd.format
        , stash = cmd.stash
        }

  cliCommandFiles Cmd_Run{..} =
    case fileTargets of
      FilesGiven files -> Just (files, \files' -> Cmd_Run{fileTargets = FilesGiven files', ..})
      _ -> Nothing

{----- hooky fix ------}

cmdFix :: CLICommandDef
cmdFix =
  CLICommandDef
    { name = "fix"
    , description = "Run hooks with autofixing enabled"
    , cmdType = Proxy @Cmd_Fix
    }

newtype Cmd_Fix = Cmd_Fix Cmd_Run

instance IsCLICommand Cmd_Fix where
  cliCommandParse = update <$> cliCommandParse
   where
    update Cmd_Run{..} = Cmd_Fix Cmd_Run{mode = Mode_Fix, ..}
  cliCommandRun = coerce $ cliCommandRun @Cmd_Run
  cliCommandFiles = coerce $ cliCommandFiles @Cmd_Run

{----- hooky lint ------}

cmdLint :: CLICommandDef
cmdLint =
  CLICommandDef
    { name = "lint"
    , description = "Run builtin hooky lint rules"
    , cmdType = Proxy @Cmd_Lint
    }

data Cmd_Lint = Cmd_Lint
  { files :: [FilePath]
  , autofix :: Bool
  }

instance IsCLICommand Cmd_Lint where
  cliCommandParse = do
    files <- parseFilesCLI
    autofix <-
      Opt.switch . mconcat $
        [ Opt.long "fix"
        , Opt.help "Whether to fix issues"
        ]
    pure Cmd_Lint{..}

  cliCommandRun cmd git config = do
    report <- runLintRules git config options
    Text.putStrLn $ renderLintReport report
    unless (lintReportSuccess report) $ do
      exitFailure
   where
    options =
      LintOptions
        { autofix = cmd.autofix
        , files = cmd.files
        }

  cliCommandFiles Cmd_Lint{..} = Just (files, \files' -> Cmd_Lint{files = files', ..})

{----- CLI Helpers -----}

-- https://github.com/pcapriotti/optparse-applicative/issues/513
cliOneOfOptional :: [Opt.Parser a] -> Opt.Parser (Maybe a)
cliOneOfOptional parsers = validate <$> traverse Opt.optional parsers
 where
  validate results =
    case catMaybes results of
      [] -> Nothing
      [a] -> Just a
      _ -> abortImpure $ "Expected exactly one of: " <> (Text.intercalate ", " . map Text.pack) optNames

  optNames = concatMap getOptNames parsers
  getOptNames :: Opt.Parser x -> [String]
  getOptNames = \case
    Opt.Internal.NilP _ -> []
    Opt.Internal.OptP opt -> [getOptName opt]
    Opt.Internal.MultP p1 p2 -> getOptNames p1 <> getOptNames p2
    Opt.Internal.AltP p1 p2 -> getOptNames p1 <> getOptNames p2
    Opt.Internal.BindP p _ -> getOptNames p
  getOptName opt =
    case Opt.Internal.optMain opt of
      Opt.Internal.OptReader names _ _ | Just name <- getName names -> name
      Opt.Internal.FlagReader names _ | Just name <- getName names -> name
      _ -> Opt.Internal.propMetaVar $ Opt.Internal.optProps opt
  getName names
    | n : _ <- [n | Opt.Internal.OptLong n <- names] = Just ("--" <> n)
    | c : _ <- [c | Opt.Internal.OptShort c <- names] = Just ['-', c]
    | otherwise = Nothing

parseFilesCLI :: Opt.Parser [FilePath]
parseFilesCLI =
  Opt.some . Opt.strArgument . mconcat $
    [ Opt.metavar "FILES"
    , Opt.help . concat $
        [ "Files to run on. Any files of the form `@f.ext` will be expanded to "
        , "the list of files in it, where `@f.ext` contains a filepath per line."
        ]
    ]

parseRunModeCLI :: Opt.Parser RunMode
parseRunModeCLI = do
  let modes = uncommas $ map renderRunMode allRunModes
      uncommas = Text.unpack . Text.intercalate ", " . map Text.pack
  fmap (fromMaybe Mode_Check) . Opt.optional $
    Opt.option (Opt.maybeReader parseRunMode) . mconcat $
      [ Opt.long "mode"
      , Opt.help $ "Mode to run hooky in during git hooks. One of: " <> modes
      ]

parseFormatCLI :: Opt.Parser OutputFormat
parseFormatCLI =
  fmap (fromMaybe Format_Minimal) . Opt.optional $
    Opt.option (Opt.maybeReader parseOutputFormat) . mconcat $
      [ Opt.long "format"
      , Opt.help $ "Format of output. One of: " <> formats
      ]
 where
  formats = uncommas $ map renderOutputFormat allOutputFormats
  uncommas = Text.unpack . Text.intercalate ", " . map Text.pack
