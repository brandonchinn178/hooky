module Hooky.Utils.Path (
  pathToText,
  writeScript,
) where

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Path (File, Path, toFilePath)
import Path.IO (getPermissions, setPermissions, setOwnerExecutable)

pathToText :: Path b t -> Text
pathToText = Text.pack . toFilePath

writeScript :: Path b File -> Text -> IO ()
writeScript file contents = do
  Text.writeFile (toFilePath file) contents
  setPermissions file . setOwnerExecutable True =<< getPermissions file
