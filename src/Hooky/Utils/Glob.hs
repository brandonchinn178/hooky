{-# LANGUAGE OverloadedStrings #-}

module Hooky.Utils.Glob (
  Glob,
  matchesGlob,
  matchesGlobs,
  toGlob,
  renderGlob,
) where

import Data.List (partition, tails)
import Data.Text (Text)
import Data.Text qualified as Text

-- | TODO: make proper data type
-- (isNegate, [Left isStarStar, Right lit])
newtype Glob = Glob (Bool, [Either Bool String])
  deriving (Eq)

instance Show Glob where
  showsPrec _ glob = showString "toGlob \"" . showString (Text.unpack $ renderGlob glob) . showString "\""

toGlob :: Text -> Glob
toGlob = Glob . parse0 . Text.unpack
 where
  parse0 = \case
    '!' : cs -> (True, parse1 cs)
    cs -> (False, parse1 cs)

  parse1 = \case
    '/' : cs -> parse2 cs
    cs -> Left True : dropLeading (Left True) (parse2 cs)

  parse2 = \case
    '*' : '*' : cs -> Left True : parse2 (dropLeading '/' cs)
    '*' : cs -> Left False : parse2 cs
    -- TODO: collapse all consecutive Rights
    c : cs -> Right [c] : parse2 cs
    [] -> []

  dropLeading x = \case
    a : as | a == x -> as
    as -> as

renderGlob :: Glob -> Text
renderGlob (Glob (isNegate, parts)) = (if isNegate then "!" else "") <> foldMap go parts
 where
  go = \case
    Left True -> "**/"
    Left False -> "*"
    Right s -> Text.pack s

matchesGlob :: Glob -> Text -> Bool
matchesGlob (Glob (isNegate, parts)) = (if isNegate then not else id) . go parts
 where
  go [] = Text.null
  go (Left True : rest) = any (go rest) . wildcardDirs
  go (Left False : rest) = any (go rest) . wildcardFile
  go (Right s : rest) = maybe False (go rest) . Text.stripPrefix (Text.pack s)

  wildcardDirs = map (Text.intercalate "/") . tails . Text.splitOn "/"
  wildcardFile fp =
    let (pre, post) = Text.breakOn "/" fp
     in map (<> post) $ Text.tails pre

matchesGlobs :: [Glob] -> Text -> Bool
matchesGlobs globs s =
  and
    [ null posGlobs || any matches posGlobs
    , null negGlobs || all matches negGlobs
    ]
 where
  matches = (`matchesGlob` s)
  (negGlobs, posGlobs) = partition (\(Glob (x, _)) -> x) globs
