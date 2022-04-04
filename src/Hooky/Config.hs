{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hooky.Config (
  Config (..),
  Check (..),
  Source (..),
  SourceReference (..),
  parseConfig,
) where

import Control.Applicative (Alternative (..), (<|>))
import Control.Monad (when, (<=<))
import Data.Bifunctor (first)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, isNothing)
import Data.Text (Text)
import Data.Text qualified as Text
import System.FilePath.Glob (Pattern)
import System.FilePath.Glob qualified as Glob
import TOML qualified

data Config = Config
  { cfgChecks :: [Check]
  , cfgSources :: Map Text Source
  , cfgExclude :: [Pattern]
  }
  deriving (Show, Eq)

-- TODO: add 'setup' field
data Check = Check
  { checkName :: Text
  , checkSource :: Maybe SourceReference
  , checkCommand :: [Text]
  , checkFiles :: [Pattern]
  }
  deriving (Show, Eq)

data Source
  = GitSource
      { gitUrl :: Text
      , gitRev :: Text
      }
  | TarSource
      { tarUrl :: Text
      }
  deriving (Show, Eq)

data SourceReference = SourceReference
  { srcRefName :: Text
  , srcRefCheck :: Text
  }
  deriving (Show, Eq)

{-- TOML parsing --}

-- TODO: better printing of error
parseConfig :: Text -> Either Text Config
parseConfig = fromTOML . TOML.Table <=< first (Text.pack . show) . TOML.parseTOML

instance FromTOML Config where
  fromTOML = withObject "Config" $ \o ->
    Config
      <$> o .:? "check" .!= []
      <*> o .:? "source" .!= Map.empty
      <*> oneOrArrayAt o "exclude"

instance FromTOML Pattern where
  fromTOML = fmap (Glob.compile . Text.unpack) . fromTOML

instance FromTOML Check where
  fromTOML = withObject "Check" $ \o -> do
    checkName <- o .: "name"

    rawSource <- o .:? "source"
    checkSource <-
      flip traverse rawSource $ \case
        TOML.String srcName -> pure $ SourceReference srcName checkName
        v@(TOML.Table _) -> fromTOML v
        v -> Left $ "Could not parse SourceReference: " <> Text.pack (show v)

    checkCommand <- o .:? "command" >>= parseCommand

    when (null checkCommand && isNothing checkSource) $
      Left "'command' or 'source' must be provided"

    checkFiles <- oneOrArrayAt o "files"

    pure Check{..}
    where
      parseCommand = \case
        Nothing -> pure []
        Just (TOML.String s)
          | ' ' `Text.elem` s -> pure ["bash", "-c", s]
          | otherwise -> pure [s]
        Just v@(TOML.List _) -> fromTOML v
        v -> Left $ "Could not parse command: " <> Text.pack (show v)

instance FromTOML Source where
  fromTOML v = flip (withObject "Source") v $ \o ->
    if
        | "github" `Map.member` o -> parseGitHubSource v
        | "git" `Map.member` o -> parseGitSource v
        | "url" `Map.member` o -> parseTarSource v
        | otherwise -> Left $ "Invalid source: " <> Text.pack (show o)
    where
      parseGitHubSource = withObject "GitHubSource" $ \o ->
        GitSource
          <$> (o .: "github" >>= githubToURL)
          <*> o .: "rev"
      parseGitSource = withObject "GitSource" $ \o ->
        GitSource
          <$> o .: "git"
          <*> o .: "rev"
      parseTarSource = withObject "TarSource" $ \o ->
        TarSource <$> o .: "url"

      githubToURL t =
        case Text.splitOn "/" t of
          [owner, repo] -> pure $ "https://github.com/" <> owner <> "/" <> repo <> ".git"
          _ -> Left $ "Invalid github repository: " <> t

instance FromTOML SourceReference where
  fromTOML = withObject "SourceReference" $ \o ->
    SourceReference
      <$> o .: "name"
      <*> o .: "check"

oneOrArrayAt :: FromTOML a => Map Text TOML.Value -> Text -> Either Text [a]
oneOrArrayAt o key = ((: []) <$> o .: key) <|> (o .:? key .!= [])

{-- FromTOML --}

-- TODO: move this to toml-parser? provide better API?
class FromTOML a where
  fromTOML :: TOML.Value -> Either Text a

instance FromTOML a => FromTOML [a] where
  fromTOML = \case
    TOML.List vs -> mapM fromTOML vs
    v -> Left $ "Expected List, got: " <> Text.pack (show v)

instance FromTOML Text where
  fromTOML = \case
    TOML.String t -> Right t
    v -> Left $ "Expected Text, got: " <> Text.pack (show v)

instance FromTOML a => FromTOML (Map Text a) where
  fromTOML = \case
    TOML.Table kvs -> traverse fromTOML $ Map.fromList kvs
    v -> Left $ "Expected Table, got: " <> Text.pack (show v)

instance FromTOML TOML.Value where
  fromTOML = Right

withObject :: Text -> (Map Text TOML.Value -> Either Text a) -> TOML.Value -> Either Text a
withObject label f = \case
  TOML.Table kvs -> f $ Map.fromList kvs
  v -> Left $ "Could not parse " <> label <> ", got: " <> Text.pack (show v)

(.:) :: FromTOML a => Map Text TOML.Value -> Text -> Either Text a
o .: key =
  maybe (Left $ "Could not find " <> key <> " in " <> Text.pack (show o)) fromTOML $
    Map.lookup key o

(.:?) :: FromTOML a => Map Text TOML.Value -> Text -> Either Text (Maybe a)
o .:? key = traverse fromTOML $ Map.lookup key o

(.!=) :: Either Text (Maybe a) -> a -> Either Text a
m .!= def = fromMaybe def <$> m

-- TODO: proper Parser type, show path in error
instance Alternative (Either Text) where
  empty = Left "Parse error"

  Right x <|> _ = Right x
  Left _ <|> x = x
