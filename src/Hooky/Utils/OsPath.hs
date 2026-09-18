module Hooky.Utils.OsPath (
  fromFilePath,
  toFilePath,
  fromText,
  toText,
  toLazyText,

  -- * IO
  listDirectoryRecur,
) where

import Control.Monad (forM)
import Data.ByteString.Short qualified as ByteString.Short
import Data.Coerce (coerce)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text

-- Needed to `coerce`

import Data.Text.Lazy (LazyText)
import Data.Text.Lazy qualified as LazyText
import Hooky.Error (abortImpure)
import System.Directory.OsPath (doesDirectoryExist, listDirectory)
import System.OsPath (OsPath, (</>))
import System.OsPath qualified as OsPath
import System.OsString qualified as OsString
import System.OsString.Internal.Types (
  OsString (..),
  PosixString (..),
 )
import UnliftIO.Exception (displayException)

fromFilePath :: FilePath -> OsPath
fromFilePath s =
  case OsPath.encodeUtf s of
    Right p -> p
    Left e -> abortImpure . Text.pack $ "Invalid path: " <> show s <> "\n" <> displayException e

toFilePath :: OsPath -> FilePath
toFilePath p =
  case OsPath.decodeUtf p of
    Right s -> s
    Left e -> abortImpure . Text.pack $ "Invalid path: " <> show p <> "\n" <> displayException e

-- https://hackage-content.haskell.org/package/unwitch-3.1.0/docs/src/Unwitch.Convert.Text.html#toOsString
fromText :: Text -> OsPath
fromText =
  case OsString.coercionToPlatformTypes of
    Left{} -> coerce . ByteString.Short.toShort . Text.encodeUtf16LE
    Right{} -> coerce . ByteString.Short.toShort . Text.encodeUtf8

toText :: OsPath -> Text
toText =
  case OsString.coercionToPlatformTypes of
    Left{} -> Text.decodeUtf16LE . ByteString.Short.fromShort . coerce
    Right{} -> Text.decodeUtf8 . ByteString.Short.fromShort . coerce

toLazyText :: OsPath -> LazyText
toLazyText = LazyText.fromStrict . toText

listDirectoryRecur :: OsPath -> IO [OsPath]
listDirectoryRecur dir = do
  names <- listDirectory dir
  fmap concat . forM names $ \name -> do
    let path = dir </> name
    isDir <- doesDirectoryExist path
    if isDir
      then listDirectoryRecur path
      else pure [path]
