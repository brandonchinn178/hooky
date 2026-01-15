{-# LANGUAGE OverloadedStrings #-}

module Hooky.Internal.Output (
  renderShell,
  renderHookStatus,
  renderHookBody,
  renderHookHeader,
  renderLogLines,
) where

import Data.Char (isSpace)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as Lazy
import Data.Text.Lazy qualified as TextL
import Hooky.Utils.Term qualified as Term

renderShell :: [Text] -> Lazy.Text
renderShell args =
  TextL.intercalate " " $
    [ TextL.fromStrict $ if Text.any isSpace s then "'" <> s <> "'" else s
    | s <- args
    ]

renderHookStatus :: Text -> Lazy.Text -> Lazy.Text
renderHookStatus name status =
  TextL.concat
    [ "╭─── "
    , status
    , " "
    , Term.bold $ TextL.fromStrict name
    , " "
    ]

renderHookBody :: [Lazy.Text] -> Lazy.Text
renderHookBody = TextL.unlines . map ("│ " <>)

renderHookHeader :: Text -> Int -> Lazy.Text
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

renderLogLines :: [Lazy.Text] -> [Lazy.Text]
renderLogLines = map Term.yellow . onHead ("═══▶ " <>)
 where
  onHead f = \case
    [] -> []
    x : xs -> f x : xs
