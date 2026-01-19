{-# LANGUAGE OverloadedStrings #-}

module Hooky.Internal.Messages (
  render,
  log,
  info,
) where

import Data.Text.Lazy (LazyText)
import Data.Text.Lazy qualified as TextL
import Data.Text.Lazy.IO qualified as TextL
import Hooky.Utils.Term qualified as Term
import Prelude hiding (log)

render :: LazyText -> LazyText
render = TextL.intercalate "\n" . onHead ("═══▶ " <>) . TextL.splitOn "\n"
 where
  onHead f = \case
    [] -> []
    x : xs -> f x : xs

log :: LazyText -> IO ()
log = TextL.putStrLn . Term.yellow . render

info :: LazyText -> IO ()
info = TextL.putStrLn . Term.lightBlue . render
