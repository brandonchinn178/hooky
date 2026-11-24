{-# LANGUAGE OverloadedStrings #-}

module Hooky.Internal.Messages (
  render,
  log,
  info,
) where

import Data.Text.Lazy (LazyText)
import Data.Text.Lazy qualified as LazyText
import Data.Text.Lazy.IO qualified as LazyText
import Hooky.Utils.Term qualified as Term
import Prelude hiding (log)

render :: LazyText -> LazyText
render = LazyText.intercalate "\n" . onHead ("═══▶ " <>) . LazyText.splitOn "\n"
 where
  onHead f = \case
    [] -> []
    x : xs -> f x : xs

log :: LazyText -> IO ()
log = LazyText.putStrLn . Term.yellow . render

info :: LazyText -> IO ()
info = LazyText.putStrLn . Term.lightBlue . render
