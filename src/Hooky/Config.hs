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
  CommandDefinition (..),
  SourceReference (..),
  parseConfig,
) where

import Control.Applicative (Alternative (..), (<|>))
import Data.Aeson (
  FromJSON (..),
  Key,
  Object,
  Value (..),
  withObject,
  (.!=),
  (.:),
  (.:?),
 )
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Parser)
import Data.Bifunctor (first)
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Yaml qualified as Yaml
import System.FilePath.Glob (Pattern)
import System.FilePath.Glob qualified as Glob

data Config = Config
  { cfgChecks :: [Check]
  , cfgSources :: Map Text Source
  , cfgExclude :: [Pattern]
  }
  deriving (Show, Eq)

-- TODO: add 'setup' field
data Check = Check
  { checkName :: Text
  , checkCommand :: CommandDefinition
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

data CommandDefinition
  = ExplicitCommand Text
  | ExplicitCommandList (NonEmpty Text)
  | ExplicitCommandShell Text
  | CommandFromSource SourceReference
  deriving (Show, Eq)

data SourceReference = SourceReference
  { srcRefName :: Text
  , srcRefCheck :: Text
  }
  deriving (Show, Eq)

{-- TOML parsing --}

parseConfig :: Text -> Either Text Config
parseConfig = first (Text.pack . Yaml.prettyPrintParseException) . Yaml.decodeEither' . Text.encodeUtf8

instance FromJSON Config where
  parseJSON = \case
    Object o -> go o
    Null -> go mempty -- empty config file
    v -> fail $ "Could not parse config: " <> show v
    where
      go o =
        Config
          <$> o .:? "checks" .!= []
          <*> o .:? "sources" .!= Map.empty
          <*> oneOrArrayAt o "exclude"

instance FromJSON Pattern where
  parseJSON = fmap (Glob.compile . Text.unpack) . parseJSON

instance FromJSON Check where
  parseJSON = withObject "Check" $ \o -> do
    checkName <- o .: "name"

    rawSource <- o .:? "source"
    source <-
      flip traverse rawSource $ \case
        String srcName -> pure $ SourceReference srcName checkName
        v@(Object _) -> parseJSON v
        v -> fail $ "Could not parse SourceReference: " <> show v

    command <- o .:? "command" >>= parseCommand
    checkCommand <-
      case (source, command) of
        (Nothing, Nothing) -> fail "'command' or 'source' must be provided"
        (Nothing, Just cmd) -> pure cmd
        (Just sourceRef, Nothing) -> pure $ CommandFromSource sourceRef
        (Just _, Just _) -> fail "Cannot specify both 'command' and 'source'"
    checkFiles <- oneOrArrayAt o "files"
    pure Check{..}
    where
      parseCommand = \case
        Nothing -> pure Nothing
        Just (String s)
          | ' ' `Text.elem` s -> pure $ Just $ ExplicitCommandShell s
          | otherwise -> pure $ Just $ ExplicitCommand s
        Just v@(Array _) -> Just . ExplicitCommandList <$> parseJSON v
        v -> fail $ "Could not parse command: " <> show v

instance FromJSON Source where
  parseJSON = withObject "Source" $ \o ->
    if
        | Just (String githubRef) <- "github" `KeyMap.lookup` o ->
            GitSource
              <$> githubToURL githubRef
              <*> o .: "rev"
        | Just (String gitUrl) <- "git" `KeyMap.lookup` o ->
            GitSource gitUrl <$> o .: "rev"
        | Just (String tarUrl) <- "url" `KeyMap.lookup` o ->
            pure $ TarSource tarUrl
        | otherwise -> fail $ "Invalid source: " <> show o
    where
      githubToURL t =
        case Text.splitOn "/" t of
          [owner, repo] -> pure $ "https://github.com/" <> owner <> "/" <> repo <> ".git"
          _ -> fail $ "Invalid github repository: " <> Text.unpack t

instance FromJSON SourceReference where
  parseJSON = withObject "SourceReference" $ \o ->
    SourceReference
      <$> o .: "name"
      <*> o .: "check"

oneOrArrayAt :: FromJSON a => Object -> Key -> Parser [a]
oneOrArrayAt o key = ((: []) <$> o .: key) <|> (o .:? key .!= [])
