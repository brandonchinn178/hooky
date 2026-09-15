{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module Hooky.Utils.Glob (
  Glob,
  matchesGlob,
  matchesGlobs,
  toGlob,
  renderGlob,
) where

import Data.List (partition)
import Data.Text (Text)
import Data.Text qualified as Text
import System.FilePath.Glob qualified as Glob

data Glob = Glob
  { original :: Text
  , pattern :: Glob.Pattern
  , invert :: Bool
  }
  deriving (Eq)

instance Show Glob where
  showsPrec _ glob = showString "toGlob " . shows (renderGlob glob)

toGlob :: Text -> Glob
toGlob s0 = parse0 . Text.unpack $ s0
 where
  parse0 = \case
    '!' : cs -> (parse1 cs){invert = True}
    cs -> parse1 cs

  parse1 = \case
    '/' : cs -> parse2 cs
    cs -> parse2 ("**/" <> cs)

  parse2 s =
    Glob
      { original = s0
      , pattern = Glob.compile s
      , invert = False
      }

renderGlob :: Glob -> Text
renderGlob = (.original)

matchesGlob :: Glob -> Text -> Bool
matchesGlob glob s = invert $ matches glob.pattern
 where
  invert = if glob.invert then not else id
  matches = flip Glob.match (Text.unpack s)

matchesGlobs :: [Glob] -> Text -> Bool
matchesGlobs globs s =
  and
    [ null posGlobs || any matches posGlobs
    , null negGlobs || all matches negGlobs
    ]
 where
  matches = (`matchesGlob` s)
  (negGlobs, posGlobs) = partition (.invert) globs
