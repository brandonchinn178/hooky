{-# LANGUAGE OverloadedStrings #-}

module Hooky.Internal.Output (
  -- * Render shell command
  renderShell,

  -- * Outputs for running hooks
  renderHookStatus,
  renderHookBody,
  renderHookHeader,

  -- * Hooky messages
  renderLogLines,
  outputLogLines,
) where

import Data.Char (isSpace)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy (LazyText)
import Data.Text.Lazy qualified as TextL
import Data.Text.Lazy.IO qualified as TextL
import Hooky.Utils.Term qualified as Term

renderShell :: [Text] -> LazyText
renderShell args =
  TextL.intercalate " " $
    [ TextL.fromStrict $ if Text.any isSpace s then "'" <> s <> "'" else s
    | s <- args
    ]

renderHookStatus :: Text -> LazyText -> LazyText
renderHookStatus name status =
  TextL.concat
    [ "╭─── "
    , status
    , " "
    , Term.bold $ TextL.fromStrict name
    , " "
    ]

renderHookBody :: [LazyText] -> LazyText
renderHookBody = TextL.unlines . map ("│ " <>)

renderHookHeader :: Text -> Int -> LazyText
renderHookHeader name time =
  TextL.concat $
    [ start
    , "["
    , TextL.pack $ map getBarChar [0 .. totalWidth - 1]
    , "]\n"
    ]
 where
  totalWidth = 6 :: Int
  barWidth = 3 :: Int

  start = renderHookStatus name (Term.yellowBG "RUNNING")

  getBarChar i =
    if any (== i) . map (`mod` totalWidth) . map (time +) $ [0 .. barWidth - 1]
      then '='
      else ' '

renderLogLines :: [LazyText] -> [LazyText]
renderLogLines = map Term.yellow . onHead ("═══▶ " <>)
 where
  onHead f = \case
    [] -> []
    x : xs -> f x : xs

outputLogLines :: LazyText -> IO ()
outputLogLines = TextL.putStr . TextL.unlines . renderLogLines . TextL.lines
