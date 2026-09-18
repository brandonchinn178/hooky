{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}

module Hooky.Internal.GitFile (
  -- * GitFileTarget
  GitFileTarget (..),
  GitFileTargetArg (..),
  parseGitFileTargetArg,

  -- * GitFile
  GitFile (..),
  GitFileSymlink (..),
  mkGitFileSymlink,
  resolveGitFiles,
) where

import Control.Monad (guard, (<=<))
import Data.Function (on)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import GHC.Records (HasField (..))
import Hooky.Error (abort)
import Hooky.Internal.Logging qualified as Logging
import Hooky.Utils.Git (GitClient)
import Hooky.Utils.Text qualified as Text
import System.Directory (
  canonicalizePath,
  doesDirectoryExist,
  doesFileExist,
  getCurrentDirectory,
  getSymbolicLinkTarget,
  pathIsSymbolicLink,
 )
import System.FilePath (
  isAbsolute,
  makeRelative,
  takeDirectory,
  (</>),
 )
import System.IO.Unsafe (unsafeInterleaveIO)
import UnliftIO.Exception (catchAny)
import Prelude hiding (lines)

-- | The ways a user might be able to specify the files to run on.
data GitFileTarget
  = FilesGiven [GitFileTargetArg]
  | FilesModified
  | FilesStaged
  | FilesAll
  | FilesPrev
  deriving (Show, Eq)

data GitFileTargetArg
  = FileArg FilePath
  | FileArgsFrom FilePath
  deriving (Show, Eq)

parseGitFileTargetArg :: String -> GitFileTargetArg
parseGitFileTargetArg = \case
  '@' : s -> FileArgsFrom s
  s -> FileArg s

-- | A file that is tracked by and can be returned by git.
--
-- `git ls-files -s` can only return the following values:
--   * 100644 - Regular file, non-executable
--   * 100755 - Regular file, executable
--   * 120000 - Symbolic link
--   * 160000 - Git submodule
--
-- We filter out git submodules entirely. It's up to the hook command / each
-- `hooky lint` rule to handle symlinks appropriately.
data GitFile
  = GitFile !FilePath
  | GitFile_Symlink !GitFileSymlink
  deriving (Show, Eq, Ord)

instance HasField "path" GitFile FilePath where
  getField = \case
    GitFile path -> path
    GitFile_Symlink link -> link.path

data GitFileSymlink = GitFileSymlink
  { path :: !FilePath
  , isDir :: Bool
  -- ^ Lazily fetched on first use
  , target :: FilePath
  -- ^ Lazily fetched on first use
  }

instance Show GitFileSymlink where
  show link = "mkGitFileSymlink " <> show link.path
instance Eq GitFileSymlink where
  (==) = (==) `on` (.path)
instance Ord GitFileSymlink where
  compare = compare `on` (.path)

mkGitFileSymlink :: FilePath -> IO GitFileSymlink
mkGitFileSymlink path = do
  isDir <- unsafeInterleaveIO $ doesDirectoryExist path
  target <- unsafeInterleaveIO $ do
    raw <- getSymbolicLinkTarget path
    -- Make the target relative to whatever `path` is relative to
    resolvePath (takeDirectory path </> raw)
  pure GitFileSymlink{..}
 where
  -- Similar to canonicalizePath, except keeps the path relative if relative
  resolvePath fp = do
    if isAbsolute fp
      then canonicalizePath fp
      else do
        cwd <- getCurrentDirectory
        makeRelative cwd <$> canonicalizePath (cwd </> fp)

resolveGitFiles :: GitClient -> GitFileTarget -> IO (Set GitFile)
resolveGitFiles git = \case
  FilesGiven files -> resolve files
  FilesModified -> getChangedFiles []
  FilesStaged -> getChangedFiles ["--staged"]
  FilesAll -> getTrackedFiles []
  FilesPrev -> getChangedFiles ["HEAD~1..HEAD"]
 where
  getChangedFiles args = do
    out <- git.query (["diff", "--raw", "--diff-filter=AMR"] <> args <> ["-z"])

    -- diff --raw should output lines in the following format:
    --   :<old-mode> <new-mode> <old-hash> <new-hash> <status>\NUL<path>\NUL
    -- if <status> is C or R, there are two \NUL-separated paths instead of one.
    let parseOutput = parseStart [] . Text.splitNULs
        parseStart acc = \case
          [] -> acc
          s : rest ->
            case parseOneLine s rest of
              Just (x, rest') -> parseStart (x : acc) rest'
              Nothing ->
                Logging.traceWarn ("Unexpected line when fetching changed files: " <> (Text.pack . show) s) $
                  parseStart acc rest
        parseOneLine s rest = do
          [oldMode0, newMode, _oldHash, _newHash, status] <- pure $ Text.words s
          (':', _oldMode) <- Text.uncons oldMode0
          (path, rest') <-
            if status `elem` ["C", "R"]
              then do
                _src : dest : rest' <- pure rest
                Just (dest, rest')
              else do
                path : rest' <- pure rest
                Just (path, rest')
          Just ((newMode, Text.unpack path), rest')

    fmap Set.fromList . mapMaybeM toGitFile . parseOutput $ out

  getTrackedFiles args = do
    lines <- Text.splitNULs <$> git.query (["ls-files", "--stage", "-z"] <> args)
    deletedFiles <- Set.fromList . map Text.unpack <$> git.getLinesFrom ["ls-files", "--deleted"]

    -- ls-files --stage should output lines in the following format:
    -- <mode> <hash> <stage>TAB<path>
    let parseLine line =
          case Text.words line of
            [mode, _hash, _stage, path] -> Just (mode, Text.unpack path)
            _ -> Logging.traceWarn ("Unexpected line when fetching tracked files: " <> (Text.pack . show) line) Nothing
        notDeleted file@(_, path) = do
          guard $ path `Set.notMember` deletedFiles
          Just file

    fmap Set.fromList . mapMaybeM toGitFile . mapMaybe (notDeleted <=< parseLine) $ lines

  resolve fileArgs = do
    files <-
      flip mconcatMapM fileArgs $ \case
        FileArg file -> pure [file]
        FileArgsFrom path -> map Text.unpack . Text.lines <$> Text.readFile path

    trackedFiles <- getTrackedFiles $ "--" : files
    untrackedFiles <-
      mconcatMapM loadUntrackedFile . Set.toList $
        Set.fromList files Set.\\ Set.map (.path) trackedFiles

    pure $ trackedFiles <> untrackedFiles

  loadUntrackedFile path =
    maybe (abort $ "File does not exist: " <> Text.pack path) pure <=< ifM $
      [ (,) (pathIsSymbolicLink path `catchAny` \_ -> pure False) $ do
          Set.singleton . GitFile_Symlink <$> mkGitFileSymlink path
      , (,) (doesFileExist path) $ do
          pure . Set.singleton $ GitFile path
      , (,) (doesDirectoryExist path) $ do
          -- All tracked files were already captured by `getTrackedFiles`, so we
          -- only need to find untracked, non-ignored files in this directory
          git.getLinesFrom ["ls-files", "--other", "--exclude-standard", path]
            >>= mconcatMapM (loadUntrackedFile . Text.unpack)
      ]

  toGitFile (mode, path) =
    case mode of
      "100644" -> pure . Just $ GitFile path
      "100755" -> pure . Just $ GitFile path
      "120000" -> Just . GitFile_Symlink <$> mkGitFileSymlink path
      "160000" -> pure Nothing
      "000000" -> pure Nothing -- deleted
      _ -> Logging.traceWarn ("Unexpected mode: " <> mode) $ pure Nothing

{----- Combinators -----}

mconcatMapM :: (Monad m, Monoid b) => (a -> m b) -> [a] -> m b
mconcatMapM f = fmap mconcat . mapM f

mapMaybeM :: (Monad m) => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM f = fmap catMaybes . mapM f

ifM :: [(IO Bool, IO a)] -> IO (Maybe a)
ifM = \case
  (cond, action) : xs -> cond >>= \p -> if p then Just <$> action else ifM xs
  [] -> pure Nothing
