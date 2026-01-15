module Hooky.Utils.Term (
  -- * Utilities for styling terminal output
  bold,
  green,
  red,
  yellow,
  redBG,
  yellowBG,
) where

import Data.Text.Lazy qualified as Lazy (Text)
import Data.Text.Lazy qualified as TextL
import System.Console.ANSI (
  ColorIntensity (..),
  ConsoleIntensity (..),
  ConsoleLayer (..),
  SGR (..),
  setSGRCode,
 )
import System.Console.ANSI qualified as ANSI

{----- Output -----}

type TextWrapper = Lazy.Text -> Lazy.Text

wrapSGR :: [SGR] -> TextWrapper
wrapSGR codes s =
  TextL.concat
    [ TextL.pack $ setSGRCode codes
    , s
    , TextL.pack $ setSGRCode [Reset]
    ]

bold :: TextWrapper
bold = wrapSGR [SetConsoleIntensity BoldIntensity]

green :: TextWrapper
green = wrapSGR [SetColor Foreground Vivid ANSI.Green]

red :: TextWrapper
red = wrapSGR [SetColor Foreground Vivid ANSI.Red]

yellow :: TextWrapper
yellow = wrapSGR [SetColor Foreground Dull ANSI.Yellow]

redBG :: TextWrapper
redBG = wrapSGR [SetColor Background Dull ANSI.Red]

yellowBG :: TextWrapper
yellowBG = wrapSGR [SetColor Background Dull ANSI.Yellow]
