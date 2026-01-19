{-# LANGUAGE OverloadedStrings #-}

module Hooky.Internal.Output (
  -- * Outputs for running hooks
  renderHookInProgressHeader,
  renderHookInProgressBody,
  renderHookReport,

  -- * Format
  OutputFormat (..),
  allOutputFormats,
  parseOutputFormat,
  renderOutputFormat,
) where

import Data.List.NonEmpty qualified as NonEmpty
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text.Lazy (LazyText)
import Data.Text.Lazy qualified as TextL
import Hooky.Utils.Term qualified as Term

renderHookStatus :: Text -> LazyText -> LazyText
renderHookStatus name status =
  TextL.concat
    [ status
    , " "
    , Term.bold $ TextL.fromStrict name
    ]

renderHookInProgressHeader :: Text -> Int -> LazyText
renderHookInProgressHeader name time =
  TextL.concat $
    [ start
    , " ["
    , TextL.pack $ map getBarChar [0 .. totalWidth - 1]
    , "]\n"
    ]
 where
  totalWidth = 6 :: Int
  barWidth = 3 :: Int

  start = "╭─── " <> renderHookStatus name (Term.yellowBG "RUNNING")

  getBarChar i =
    if any (== i) . map (`mod` totalWidth) . map (time +) $ [0 .. barWidth - 1]
      then '='
      else ' '

renderHookInProgressBody :: [LazyText] -> LazyText
renderHookInProgressBody = TextL.unlines . map ("│ " <>)

renderHookReport :: Text -> LazyText -> [LazyText] -> LazyText -> LazyText
renderHookReport name status output duration =
  TextL.intercalate "\n" $
    (start <> "─── " <> renderHookStatus name status <> " " <> Term.gray ("(duration: " <> duration <> ")"))
      : body
 where
  start = if null output then "◈" else "╭"
  body =
    case NonEmpty.nonEmpty output of
      Nothing -> []
      Just outputNE ->
        let (middle, end) = (NonEmpty.init outputNE, NonEmpty.last outputNE)
         in map ("│ " <>) middle <> ["◈ " <> end]

{----- OutputFormat -----}

data OutputFormat = Format_Minimal | Format_Full | Format_Verbose
  deriving (Show, Eq, Ord, Enum, Bounded)

allOutputFormats :: [OutputFormat]
allOutputFormats = [minBound .. maxBound]

parseOutputFormat :: String -> Maybe OutputFormat
parseOutputFormat = flip Map.lookup x
 where
  x = Map.fromList [(renderOutputFormat m, m) | m <- allOutputFormats]

renderOutputFormat :: OutputFormat -> String
renderOutputFormat = \case
  Format_Minimal -> "minimal"
  Format_Full -> "full"
  Format_Verbose -> "verbose"
