{-# LANGUAGE OverloadedStrings #-}

module Hooky.Utils.Text (
  splitNULs,
) where

import Data.Text (Text)
import Data.Text qualified as Text

-- | Like 'Text.words', except splits '\NUL'
splitNULs :: Text -> [Text]
splitNULs = filter (not . Text.null) . Text.splitOn "\NUL"
