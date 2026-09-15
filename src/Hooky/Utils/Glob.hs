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
  , patterns :: [Glob.Pattern]
  , invert :: Bool
  }
  deriving (Eq)

instance Show Glob where
  showsPrec _ glob = showString "toGlob " . shows (renderGlob glob)

toGlob :: Text -> Glob
toGlob s0 = parse0 s0
 where
  parse0 s =
    case Text.uncons s of
      Just ('!', s') -> (parse1 s'){invert = True}
      _ -> parse1 s

  parse1 s =
    case Text.uncons s of
      Just ('/', s') -> parse2 s'
      _ -> parse2 ("**/" <> s)

  parse2 s =
    Glob
      { original = s0
      , patterns = map (Glob.compile . Text.unpack) $ expandBraces s
      , invert = False
      }

  -- Glob doesn't support braces, so we have to manually expand
  -- https://github.com/Deewiant/glob/issues/32
  expandBraces s
    | Just (pre, s') <- breakAt "{" s
    , Just (braced, post) <- breakAt "}" s' =
        [ pre <> braced' <> post'
        | braced' <- Text.splitOn "," braced
        , post' <- expandBraces post
        ]
    | otherwise = [s]

  breakAt sep s =
    let (pre, post) = Text.breakOn sep s
     in (pre,) <$> Text.stripPrefix sep post

renderGlob :: Glob -> Text
renderGlob = (.original)

matchesGlob :: Glob -> Text -> Bool
matchesGlob glob s = invert $ any matches glob.patterns
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
