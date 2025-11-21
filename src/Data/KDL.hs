{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | TODO: Break out into kdl-hs
module Data.KDL (
  Document,
  Ann (..),
  AnnNode,
  Node (..),
  AnnValue,
  Value (..),
  Identifier,

  -- * AnnNode helpers
  nodeName,
  nodeArg,
  nodeArgs,
  nodeProps,
  nodeChildren,

  -- * Node list helpers
  findNode,
  findNodes,
  getArgAt,
  getArgsAt,
  getDashChildren,

  -- * Decode
  Decoder,
  runDecoder,
  unexpectedValue,
  fail,

  -- ** API
  decodeWith,
  decodeFileWith,
  DecodeValue (..),
  decodeAnnValue,
  decodeArgAt,
  decodeArgsAt,
  decodeArg,
  decodeArgs,
  decodeProps,

  -- * Low-level parse
  parse,
  parseFile,
) where

import Control.Monad ((<=<))
import Data.Int (Int64)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe)
import Data.Proxy (Proxy (..))
import Data.Scientific (Scientific)
import Data.Scientific qualified as Scientific
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import KDL qualified
import Prelude hiding (fail)

type Document = [AnnNode]
type Identifier = Text

data Ann a = Ann
  { ann :: Maybe Identifier
  , obj :: a
  }
  deriving (Show, Eq)

type AnnNode = Ann Node
data Node = Node
  { name :: Identifier
  , args :: [AnnValue]
  , props :: Map Identifier AnnValue
  , children :: [AnnNode]
  }
  deriving (Show, Eq)

type AnnValue = Ann Value
data Value
  = Text Text
  | Number Scientific
  | Bool Bool
  | Null
  deriving (Show, Eq)

{----- Parsing -----}

parse :: Text -> Either Text Document
parse input =
  -- TODO: Implement our own parser that satisfies 2.0.0
  case KDL.parse KDL.document "" input of
    Left e -> Left . Text.pack . KDL.errorBundlePretty $ e
    Right (KDL.Document nodes) -> Right $ map convertNode nodes
  where
    convertNode node =
      Ann
        { ann = convertIdentifier <$> KDL.nodeAnn node
        , obj =
            Node
              { name = convertIdentifier $ KDL.nodeName node
              , args = map convertValue $ KDL.nodeArgs node
              , props = Map.mapKeys convertIdentifier . Map.map convertValue $ KDL.nodeProps node
              , children = map convertNode $ KDL.nodeChildren node
              }
        }

    convertIdentifier (KDL.Identifier s) = s

    convertValue KDL.Value{..} =
      Ann
        { ann = convertIdentifier <$> valueAnn
        , obj =
            case valueExp of
              KDL.StringValue s -> Text s
              KDL.IntegerValue x -> Number (fromInteger x)
              KDL.SciValue x -> Number x
              KDL.BooleanValue x -> Bool x
              KDL.NullValue -> Null
        }

parseFile :: FilePath -> IO (Either Text Document)
parseFile = fmap parse . Text.readFile

{----- Helpers -----}

findNode :: Identifier -> [AnnNode] -> Maybe AnnNode
findNode name = listToMaybe . findNodes name

findNodes :: Identifier -> [AnnNode] -> [AnnNode]
findNodes name = filter ((== name) . nodeName)

getArgAt :: Identifier -> [AnnNode] -> Maybe AnnValue
getArgAt name = listToMaybe . getArgsAt name

getArgsAt :: Identifier -> [AnnNode] -> [AnnValue]
getArgsAt name = maybe [] nodeArgs . findNode name

-- | Helper for the convention where dashes are used as array keys:
--
-- @
-- foo {
--   - 1
--   - 2
--   - #false
-- }
-- @
getDashChildren :: Identifier -> [AnnNode] -> [AnnNode]
getDashChildren name = maybe [] (findNodes "-" . nodeChildren) . findNode name

nodeName :: AnnNode -> Identifier
nodeName Ann{obj = Node{name}} = name

nodeArg :: AnnNode -> Maybe AnnValue
nodeArg = listToMaybe . nodeArgs

nodeArgs :: AnnNode -> [AnnValue]
nodeArgs Ann{obj = Node{args}} = args

nodeProps :: AnnNode -> Map Identifier AnnValue
nodeProps Ann{obj = Node{props}} = props

nodeChildren :: AnnNode -> [AnnNode]
nodeChildren Ann{obj = Node{children}} = children

{----- Decoder -----}

newtype Decoder a = Decoder (Either Text a)
  deriving (Functor, Applicative, Monad)

runDecoder :: Decoder a -> Either Text a
runDecoder (Decoder x) = x

unexpectedValue :: Text -> AnnValue -> Decoder a
unexpectedValue expectedType actual = fail $ "Expected '" <> expectedType <> "', got: " <> (Text.pack . show) actual

fail :: Text -> Decoder a
fail = Decoder . Left

{----- Decode API -----}

decodeWith :: (Document -> Decoder a) -> Text -> Either Text a
decodeWith decoder = runDecoder . decoder <=< parse

decodeFileWith :: (Document -> Decoder a) -> FilePath -> IO (Either Text a)
decodeFileWith decoder = fmap (runDecoder . decoder =<<) . parseFile

decodeArgAt :: DecodeValue a => Identifier -> [AnnNode] -> Decoder (Maybe a)
decodeArgAt name = maybe (pure Nothing) decodeArg . findNode name

decodeArgsAt :: DecodeValue a => Identifier -> [AnnNode] -> Decoder [a]
decodeArgsAt name = maybe (pure []) decodeArgs . findNode name

decodeArg :: DecodeValue a => AnnNode -> Decoder (Maybe a)
decodeArg = traverse decodeAnnValue . listToMaybe . nodeArgs

decodeArgs :: DecodeValue a => AnnNode -> Decoder [a]
decodeArgs = mapM decodeAnnValue . nodeArgs

decodeProps :: DecodeValue a => AnnNode -> Decoder (Map Identifier a)
decodeProps = traverse decodeAnnValue . nodeProps

{----- DecodeValue -----}

decodeAnnValue :: forall a. DecodeValue a => AnnValue -> Decoder a
decodeAnnValue = \case
  Ann{ann = Just givenAnn} | (not . null) validAnns && givenAnn `notElem` validAnns ->
    Decoder (Left $ "Value has type '" <> givenAnn <> "', expected: " <> (Text.pack . show) validAnns)
  v -> decodeValue v
  where
    validAnns = validTypeAnns (Proxy @a)

class DecodeValue a where
  validTypeAnns :: Proxy a -> [Text]
  validTypeAnns _ = []

  decodeValue :: AnnValue -> Decoder a

instance DecodeValue AnnValue where
  decodeValue = pure
instance DecodeValue Value where
  decodeValue Ann{obj} = pure obj
instance DecodeValue Text where
  validTypeAnns _ = ["string", "text"]
  decodeValue = \case
    Ann{obj = Text x} -> pure x
    v -> unexpectedValue "string" v
-- TODO: Add Word8, Int8, ...
instance DecodeValue Integer where
  validTypeAnns _ = ["i8", "i16", "i32", "i64", "i128", "u8", "u16", "u32", "u64", "u128", "isize", "usize"]
  decodeValue = \case
    Ann{obj = Number x} | Scientific.isInteger x ->
      maybe (fail $ "Found large number, aborting: " <> (Text.pack . show) x) (pure . toInteger) $ Scientific.toBoundedInteger @Int64 x
    v -> unexpectedValue "integer" v
-- TODO: Add Double, Float, Rational
instance DecodeValue Scientific where
  validTypeAnns _ = ["f32", "f64", "decimal64", "decimal128"]
  decodeValue = \case
    Ann{obj = Number x} -> pure x
    v -> unexpectedValue "number" v
instance DecodeValue Bool where
  decodeValue = \case
    Ann{obj = Bool x} -> pure x
    v -> unexpectedValue "boolean" v
instance DecodeValue a => DecodeValue (Maybe a) where
  validTypeAnns _ = validTypeAnns (Proxy @a)
  decodeValue = \case
    Ann{obj = Null} -> pure Nothing
    v -> Just <$> decodeValue v
