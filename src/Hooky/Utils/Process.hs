module Hooky.Utils.Process (
  readProcessText,
  readProcessText_,
) where

import Control.Monad.IO.Class (MonadIO)
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as ByteStringL
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import System.Process.Typed (ExitCode, ProcessConfig, readProcess, readProcess_)

readProcessText :: MonadIO m => ProcessConfig stdin _stdout _stderr -> m (ExitCode, Text, Text)
readProcessText = fmap (\(code, stdout, stderr) -> (code, fromBytes stdout, fromBytes stderr)) . readProcess

readProcessText_ :: MonadIO m => ProcessConfig stdin _stdout _stderr -> m (Text, Text)
readProcessText_ = fmap (\(stdout, stderr) -> (fromBytes stdout, fromBytes stderr)) . readProcess_

fromBytes :: ByteString -> Text
fromBytes = Text.strip . Text.decodeUtf8 . ByteStringL.toStrict
