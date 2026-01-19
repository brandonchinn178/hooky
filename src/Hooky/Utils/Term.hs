module Hooky.Utils.Term (
  -- * Utilities for styling terminal output
  TextWrapper,
  bold,
  green,
  lightBlue,
  red,
  yellow,
  gray,
  greenBG,
  redBG,
  yellowBG,
) where

import Data.Text.Lazy (LazyText)
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

type TextWrapper = LazyText -> LazyText

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

lightBlue :: TextWrapper
lightBlue = wrapSGR [SetPaletteColor Foreground 81]

red :: TextWrapper
red = wrapSGR [SetColor Foreground Vivid ANSI.Red]

yellow :: TextWrapper
yellow = wrapSGR [SetColor Foreground Dull ANSI.Yellow]

gray :: TextWrapper
gray = wrapSGR [SetPaletteColor Foreground 8]

greenBG :: TextWrapper
greenBG = wrapSGR [SetColor Background Dull ANSI.Green]

redBG :: TextWrapper
redBG = wrapSGR [SetColor Background Dull ANSI.Red]

yellowBG :: TextWrapper
yellowBG = wrapSGR [SetColor Background Dull ANSI.Yellow]
