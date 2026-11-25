{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module Hooky.Utils.CSI (
  strip,
  stripWith,

  -- * Options
  StripOpts (..),
  keepColor,
) where

import Control.Applicative ((<|>))
import Data.Char (chr)
import Data.Foldable1 qualified as Foldable1
import Data.Function (on)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (fromMaybe, mapMaybe)
import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy (LazyText)
import Data.Text.Lazy qualified as LazyText
import GHC.Records (HasField (..))

data StripOpts = StripOpts
  { mKeepColor :: Maybe Bool
  }

instance Semigroup StripOpts where
  opts1 <> opts2 =
    StripOpts
      { mKeepColor = opts1.mKeepColor <|> opts2.mKeepColor
      }
instance Monoid StripOpts where
  mempty =
    StripOpts
      { mKeepColor = Nothing
      }

instance HasField "keepColor" StripOpts Bool where
  getField opts = fromMaybe False opts.mKeepColor

keepColor :: StripOpts
keepColor = mempty{mKeepColor = Just True}

strip :: (TextLike s) => s -> s
strip = stripWith mempty
{-# INLINE strip #-}

stripWith :: (TextLike s) => StripOpts -> s -> s
stripWith opts s0 =
  -- In the happy path, s0 has no control characters; do a quick check
  -- to avoid overhead
  if not $ any (\d -> contains d s0) ['\ESC', '\x9B']
    then s0
    -- Convert to/from LazyText so that the implementation
    -- only needs to use lazy text, and so that concatenations
    -- are performant
    else textFromLazy . go0 . textToLazy $ s0
 where
  go0 s =
    case breakOnOneOf ["\ESC[", "\x9B"] s of
      Nothing -> s
      Just (pre, delim, post) -> pre <> go1 delim post
  go1 buf s =
    case LazyText.span (inRange 0x30 0x3F) s of
      (params, s') -> go2 (buf <> params) s'
  go2 buf s =
    case LazyText.span (inRange 0x20 0x2F) s of
      (params, s') -> go3 (buf <> params) s'
  go3 buf s =
    case LazyText.uncons s of
      Just (c, s')
        | inRange 0x40 0x7E c ->
            let shouldStrip =
                  case c of
                    'm' | opts.keepColor -> False
                    _ -> True
             in if shouldStrip
                  then go0 s'
                  else buf <> LazyText.singleton c <> go0 s'
      -- Invalid CSI; always strip
      _ -> go0 s

  -- Quickly check if character is in Text; uses memchr
  contains c s =
    case textBreakOn (textSingleton c) s of
      (_, "") -> False
      _ -> True

  inRange lo hi c = chr lo <= c && c <= chr hi

  breakOnOneOf delims s =
    fmap (Foldable1.minimumBy (compare `on` fst3))
      . NonEmpty.nonEmpty
      . mapMaybe (\d -> breakOn d s)
      $ delims
  breakOn delim s =
    let (pre, post) = LazyText.breakOn delim s
     in (pre,delim,) <$> LazyText.stripPrefix delim post
  fst3 (a, _, _) = a
{-# INLINE stripWith #-}

class (IsString s, Eq s) => TextLike s where
  textBreakOn :: s -> s -> (s, s)
  textSingleton :: Char -> s
  textFromLazy :: LazyText -> s
  textToLazy :: s -> LazyText
instance TextLike Text where
  textBreakOn = Text.breakOn
  textSingleton = Text.singleton
  textFromLazy = LazyText.toStrict
  textToLazy = LazyText.fromStrict
instance TextLike LazyText where
  textBreakOn = LazyText.breakOn
  textSingleton = LazyText.singleton
  textFromLazy = id
  textToLazy = id
