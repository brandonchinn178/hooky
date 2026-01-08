module Hooky.Error (
  HookyError (..),
  abort,
  abortImpure,
) where

import Data.Text (Text)
import Data.Text qualified as Text
import UnliftIO.Exception (Exception (..), impureThrow, throwIO)

data HookyError = HookyError Text
  deriving (Show)

instance Exception HookyError where
  displayException (HookyError msg) = Text.unpack msg

abort :: Text -> IO a
abort = throwIO . HookyError

abortImpure :: Text -> a
abortImpure = impureThrow . HookyError
