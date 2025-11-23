{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

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
  DecodeResult,
  fromDecodeResult,
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

  -- ** API v2
  fail_v2,
  lift_v2,
  withDecoder_v2,
  decodeWith_v2,
  decodeFileWith_v2,
  count_v2,
  atLeast_v2,
  atMost_v2,
  many_v2,
  some_v2,
  optional_v2,
  setDefault_v2,
  required_v2,
  document_v2,
  node_v2,
  children_v2,
  dashChildren_v2,
  argAt_v2,
  arg_v2,
  value_v2,
  any_v2,
  text_v2,
  number_v2,
  bool_v2,
  null_v2,
  oneOf_v2,
  DecodeValue_v2 (..),
  renderDecodeError,
  DecodeM_v2,
  DecodeError_v2,
  DecodeArrow_v2,
  Decoder_v2,
  DocumentDecoder_v2,
  NodesDecoder_v2,
  NodeDecoder_v2,
  ValueDecoder_v2,
  BaseValueDecoder_v2,

  -- * Low-level parse
  parse,
  parseFile,
) where

import Control.Applicative (Alternative (..))
import Control.Arrow (Arrow (..), ArrowChoice (..), (>>>))
import Control.Category qualified as Category
import Control.Monad (forM, unless, when, (<=<), (>=>))
import Data.Int (Int64)
import Data.List (partition)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Proxy (Proxy (..))
import Data.Scientific (Scientific)
import Data.Scientific qualified as Scientific
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Typeable (Typeable, TypeRep, typeRep)
import Numeric.Natural (Natural)
import KDL qualified
import Prelude hiding (fail, max, min)

type Document = [AnnNode]
type Identifier = Text

data Ann a = Ann
  { ann :: Maybe Identifier
  , obj :: a
  }
  deriving (Show, Eq)

renderAnn :: (a -> Text) -> Ann a -> Text
renderAnn renderObj Ann{..} = maybe "" parens ann <> renderObj obj
  where
    parens s = "(" <> s <> ")"

-- TODO: Rename Node => BaseNode, AnnNode => Node
type AnnNode = Ann Node
data Node = Node
  { name :: Identifier
  , args :: [AnnValue]
  , props :: Map Identifier AnnValue
  , children :: [AnnNode]
  }
  deriving (Show, Eq)

-- TODO: Rename Value => BaseValue, AnnValue => Value
type AnnValue = Ann Value
data Value
  = Text Text
  | Number Scientific
  | Bool Bool
  | Null
  deriving (Show, Eq)

renderAnnValue :: AnnValue -> Text
renderAnnValue = renderAnn renderValue

renderValue :: Value -> Text
renderValue = \case
  Text s -> "\"" <> s <> "\""
  Number x -> (Text.pack . show) x
  Bool b -> if b then "#true" else "#false"
  Null -> "#null"

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
nodeName = (.obj.name)

nodeArg :: AnnNode -> Maybe AnnValue
nodeArg = listToMaybe . nodeArgs

nodeArgs :: AnnNode -> [AnnValue]
nodeArgs = (.obj.args)

nodeProps :: AnnNode -> Map Identifier AnnValue
nodeProps = (.obj.props)

nodeChildren :: AnnNode -> [AnnNode]
nodeChildren = (.obj.children)

{----- Decode API -----}

decodeWith :: (Document -> DecodeResult a) -> Text -> Either Text a
decodeWith decoder = fromDecodeResult . decoder <=< parse

decodeFileWith :: (Document -> DecodeResult a) -> FilePath -> IO (Either Text a)
decodeFileWith decoder = fmap (fromDecodeResult . decoder =<<) . parseFile

decodeArgAt :: DecodeValue a => Identifier -> [AnnNode] -> DecodeResult (Maybe a)
decodeArgAt name = maybe (pure Nothing) decodeArg . findNode name

decodeArgsAt :: DecodeValue a => Identifier -> [AnnNode] -> DecodeResult [a]
decodeArgsAt name = maybe (pure []) decodeArgs . findNode name

decodeArg :: DecodeValue a => AnnNode -> DecodeResult (Maybe a)
decodeArg = traverse decodeAnnValue . listToMaybe . nodeArgs

decodeArgs :: DecodeValue a => AnnNode -> DecodeResult [a]
decodeArgs = mapM decodeAnnValue . nodeArgs

decodeProps :: DecodeValue a => AnnNode -> DecodeResult (Map Identifier a)
decodeProps = traverse decodeAnnValue . nodeProps

{----- DecodeResult -----}

newtype DecodeResult a = DecodeResult (Either Text a)
  deriving (Functor, Applicative, Monad)

fromDecodeResult :: DecodeResult a -> Either Text a
fromDecodeResult (DecodeResult x) = x

unexpectedValue :: Text -> AnnValue -> DecodeResult a
unexpectedValue expectedType actual = fail $ "Expected '" <> expectedType <> "', got: " <> renderAnnValue actual

fail :: Text -> DecodeResult a
fail = DecodeResult . Left

{----- DecodeValue -----}

decodeAnnValue :: forall a. DecodeValue a => AnnValue -> DecodeResult a
decodeAnnValue = \case
  Ann{ann = Just givenAnn} | (not . null) validAnns && givenAnn `notElem` validAnns ->
    fail $ "Value has type '" <> givenAnn <> "', expected: " <> (Text.pack . show) validAnns
  v -> decodeValue v
  where
    validAnns = validTypeAnns (Proxy @a)

class DecodeValue a where
  validTypeAnns :: Proxy a -> [Text]
  validTypeAnns _ = []

  decodeValue :: AnnValue -> DecodeResult a

instance DecodeValue AnnValue where
  decodeValue = pure
instance DecodeValue Value where
  decodeValue = pure . (.obj)
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

{----- Decode API -----}

data DecodeM_v2 a = DecodeM_v2 (forall r. (Context -> BaseDecodeError -> r) -> (a -> r) -> r)
data DecodeError_v2 = DecodeError_v2 Context BaseDecodeError
  deriving (Show, Eq)
data BaseDecodeError
  = DecodeError_Custom Text
  | DecodeError_ParseError Text
  | DecodeError_CountTooFew { minCount :: Natural, actualCount :: Natural }
  | DecodeError_CountTooMany { maxCount :: Natural, actualCount :: Natural }
  | DecodeError_MismatchedAnn { givenAnn :: Identifier, validAnns :: [Identifier] }
  | DecodeError_AnnValueDecodeFail { typeHint :: TypeRep, annValue :: AnnValue }
  | DecodeError_ValueDecodeFail { expectedType :: Text, value :: Value }
  | DecodeError_DuplicateNodeDefs [Identifier]
  | DecodeError_MultipleVarNodes
  | DecodeError_UnknownNodes [AnnNode]
  | DecodeError_UnusedArgs [AnnValue]
  deriving (Show, Eq)

instance Functor DecodeM_v2 where
  fmap f (DecodeM_v2 k) = DecodeM_v2 $ \onFail onSuccess -> k onFail (onSuccess . f)
instance Applicative DecodeM_v2 where
  pure x = DecodeM_v2 $ \_ onSuccess -> onSuccess x
  DecodeM_v2 kf <*> DecodeM_v2 ka = DecodeM_v2 $ \onFail onSuccess ->
    kf onFail $ \f -> ka onFail (onSuccess . f)
instance Monad DecodeM_v2 where
  DecodeM_v2 ka >>= k = DecodeM_v2 $ \onFail onSuccess ->
    ka onFail $ \a -> let DecodeM_v2 kb = k a in kb onFail onSuccess
instance Alternative DecodeM_v2 where
  empty = fail_v2 "<empty>"
  DecodeM_v2 k1 <|> DecodeM_v2 k2 = DecodeM_v2 $ \onFail onSuccess -> k1 (\_ _ -> k2 onFail onSuccess) onSuccess

decodeThrow :: BaseDecodeError -> DecodeM_v2 a
decodeThrow e = DecodeM_v2 $ \onFail _ -> onFail [] e

fail_v2 :: Text -> DecodeM_v2 a
fail_v2 = decodeThrow . DecodeError_Custom

runDecodeM :: DecodeM_v2 a -> Either DecodeError_v2 a
runDecodeM (DecodeM_v2 f) = f (\ctx msg -> Left $ DecodeError_v2 ctx msg) Right

renderDecodeError :: DecodeError_v2 -> Text
renderDecodeError (DecodeError_v2 ctx e) = "At: " <> (Text.pack . show) ctx <> "\n" <> (Text.pack . show) e -- TODO: render pretty

data DecodeArrow_v2 s i a b = DecodeArrow_v2{schema :: s, run :: (i, a) -> DecodeM_v2 (i, b)}
type Decoder_v2 a b = DecodeArrow_v2 (SchemaOf a) a () b
type ValidatedDecoder_v2 a b = DecodeArrow_v2 (Validated (SchemaOf a)) a () b
type DocumentDecoder_v2 a = ValidatedDecoder_v2 [AnnNode] a
type NodesDecoder_v2 a = Decoder_v2 [AnnNode] a
type NodeDecoder_v2 a = Decoder_v2 AnnNode a
type ValueDecoder_v2 a = Decoder_v2 AnnValue a
type BaseValueDecoder_v2 a = Decoder_v2 Value a

type family SchemaOf a where
  SchemaOf [AnnNode] = NodesSchema
  SchemaOf AnnNode = NodeSchema
  SchemaOf AnnValue = ValueSchema
  SchemaOf Value = BaseValueSchema
  SchemaOf (Validated a) = Validated (SchemaOf a)

instance Monoid s => Category.Category (DecodeArrow_v2 s i) where
  id = lift_v2 pure
  DecodeArrow_v2 s1 bc . DecodeArrow_v2 s2 ab = DecodeArrow_v2 (s1 <> s2) $ ab >=> bc
instance Monoid s => Arrow (DecodeArrow_v2 s i) where
  arr f = lift_v2 (pure . f)
  DecodeArrow_v2 s1 bc *** DecodeArrow_v2 s2 bc' =
    DecodeArrow_v2 (s1 <> s2) $ \(i0, (b, b')) -> do
      (i1, c) <- bc (i0, b)
      (i2, c') <- bc' (i1, b')
      pure (i2, (c, c'))
instance Monoid s => ArrowChoice (DecodeArrow_v2 s i) where
  DecodeArrow_v2 s1 bc +++ DecodeArrow_v2 s2 bc' =
    DecodeArrow_v2 (s1 <> s2) $ \case
      (i, Left b) -> (fmap . fmap) Left $ bc (i, b)
      (i, Right b') -> (fmap . fmap) Right $ bc' (i, b')
instance Functor (DecodeArrow_v2 s i a) where
  fmap f (DecodeArrow_v2 schema run) = DecodeArrow_v2 schema $ ((fmap . fmap) f . run)
instance Monoid s => Applicative (DecodeArrow_v2 s i a) where
  pure = arr . const
  DecodeArrow_v2 s1 kf <*> DecodeArrow_v2 s2 kx =
    DecodeArrow_v2 (s1 <> s2) $ \(i0, a) -> do
      (i1, f) <- kf (i0, a)
      (i2, x) <- kx (i1, a)
      pure (i2, f x)
instance Monoid s => Alternative (DecodeArrow_v2 s i a) where
  empty = lift_v2 $ \_ -> empty
  DecodeArrow_v2 s1 run1 <|> DecodeArrow_v2 s2 run2 = DecodeArrow_v2 (s1 <> s2) $ \x -> run1 x <|> run2 x

lift_v2 :: Monoid s => (a -> DecodeM_v2 b) -> DecodeArrow_v2 s i a b
lift_v2 run = DecodeArrow_v2 mempty $ \(i, a) -> (i,) <$> run a

withDecoder_v2 :: Monoid s => DecodeArrow_v2 s i a b -> (b -> DecodeM_v2 c) -> DecodeArrow_v2 s i a c
withDecoder_v2 decoder f = decoder >>> lift_v2 f

type Context = [ContextItem]
data ContextItem
  = ContextNode Identifier
  | ContextArgs
  | ContextIndex Natural
  | ContextProp Identifier
  deriving (Show, Eq)

addContext :: ContextItem -> DecodeM_v2 a -> DecodeM_v2 a
addContext ctxItem (DecodeM_v2 f) = DecodeM_v2 $ \onFail onSuccess -> f (onFail . (ctxItem :)) onSuccess

type NodesSchema = MapSchema Identifier (ListSchema (Validated NodeSchema))
data NodeSchema = NodeSchema
  { validAnns :: Maybe [Identifier]
  , argSchemas :: [ListSchema ValueSchema]
  , propSchemas :: MapSchema Identifier (MaybeSchema ValueSchema)
  , childSchemas :: NodesSchema
  }
  deriving (Show, Eq)
instance Semigroup NodeSchema where
  s1 <> s2 =
    let NodeSchema s1_x1 s1_x2 s1_x3 s1_x4 = s1
        NodeSchema s2_x1 s2_x2 s2_x3 s2_x4 = s2
     in NodeSchema
          (s1_x1 <> s2_x1)
          (s1_x2 <> s2_x2)
          (s1_x3 <> s2_x3)
          (s1_x4 <> s2_x4)
instance Monoid NodeSchema where
  mempty = NodeSchema Nothing [] mempty mempty
data ValueSchema = ValueSchema
  { typeHint :: Maybe TypeRep
  , validAnns :: Maybe [Identifier]
  , validSchemas :: Maybe BaseValueSchema
  }
  deriving (Show, Eq)
instance Semigroup ValueSchema where
  s1 <> s2 =
    let ValueSchema s1_x1 s1_x2 s1_x3 = s1
        ValueSchema s2_x1 s2_x2 s2_x3 = s2
     in ValueSchema
          (s1_x1 <|> s2_x1)
          (s1_x2 <> s2_x2)
          (s1_x3 <> s2_x3)
instance Monoid ValueSchema where
  mempty = ValueSchema Nothing Nothing Nothing
type BaseValueSchema = Set ValueTypeSchema
data ValueTypeSchema
  = TextSchema
  | NumberSchema
  | BoolSchema
  | NullSchema
  deriving (Show, Eq, Ord, Enum, Bounded)
type MapSchema k v = [MapItemSchema k v]
data MapItemSchema k v
  = MapItemSchema_ExpectKey k v
  | MapItemSchema_VarKeys v
  deriving (Show, Eq)
data ListSchema a = ListSchema
  { min :: Natural
  , max :: Maybe Natural
  , itemSchema :: a
  }
  deriving (Show, Eq)
data MaybeSchema a = MaybeSchema
  { required :: Bool
  , itemSchema :: a
  }
  deriving (Show, Eq)

decodeWith_v2 :: DocumentDecoder_v2 a -> Text -> Either DecodeError_v2 a
decodeWith_v2 decoder = decodeFromParseResult decoder . parse

decodeFileWith_v2 :: DocumentDecoder_v2 a -> FilePath -> IO (Either DecodeError_v2 a)
decodeFileWith_v2 decoder = fmap (decodeFromParseResult decoder) . parseFile

decodeFromParseResult :: DocumentDecoder_v2 a -> Either Text Document -> Either DecodeError_v2 a
decodeFromParseResult decoder = runDecodeM . \case
  Left e -> decodeThrow $ DecodeError_ParseError e
  Right doc -> runDecoder_v2 decoder doc

runDecoder_v2 :: DecodeArrow_v2 s a () b -> a -> DecodeM_v2 b
runDecoder_v2 decoder a = snd <$> decoder.run (a, ())

newtype Validated a = Validated {unwrap :: a}
  deriving (Show, Eq, Semigroup, Monoid)

validateNodes :: Decoder_v2 [AnnNode] a -> ValidatedDecoder_v2 [AnnNode] a
validateNodes decoder =
  DecodeArrow_v2 (Validated decoder.schema) $ \(nodes, ()) -> do
    (nodes', result) <- decoder.run (nodes, ())
    checkAllNodesDecoded decoder.schema nodes'
    pure (nodes', result)

checkAllNodesDecoded :: NodesSchema -> [AnnNode] -> DecodeM_v2 ()
checkAllNodesDecoded schema nodes = do
  let expectedKeys = [k | MapItemSchema_ExpectKey k _ <- schema]
  case getDups expectedKeys of
    [] -> pure ()
    -- TODO: Can we throw this statically-verifiable error earlier?
    dups -> decodeThrow $ DecodeError_DuplicateNodeDefs dups

  let numVarKeys = sum [1 | MapItemSchema_VarKeys _ <- schema] :: Int
  when (numVarKeys > 1) $
    -- TODO: Can we throw this statically-verifiable error earlier?
    decodeThrow DecodeError_MultipleVarNodes

  unless (null nodes) $
    decodeThrow (DecodeError_UnknownNodes nodes)

  -- Intentionally don't recurse, children should already be validated
  pure ()
  where
    getDups xs = Map.keys $ Map.filter (> 1) $ Map.fromListWith (+) [(x, 1 :: Int) | x <- xs]

validateNode :: Decoder_v2 AnnNode a -> ValidatedDecoder_v2 AnnNode a
validateNode decoder =
  DecodeArrow_v2 (Validated decoder.schema) $ \(node, ()) -> do
    (node', result) <- decoder.run (node, ())
    checkFullNodeDecoded decoder.schema node'
    pure (node', result)

checkFullNodeDecoded :: NodeSchema -> AnnNode -> DecodeM_v2 ()
checkFullNodeDecoded schema node = do
  unless (null node.obj.args) $
    decodeThrow (DecodeError_UnusedArgs node.obj.args)
  -- TODO: check props
  checkAllNodesDecoded schema.childSchemas (nodeChildren node)

type CountSpec = (Natural, Maybe Natural)
newtype WithCountSpec decoder = WithCountSpec{run :: CountSpec -> decoder}

mkListSchema :: CountSpec -> s -> ListSchema s
mkListSchema (min, max) itemSchema = ListSchema{..}

splitCount :: CountSpec -> [a] -> DecodeM_v2 ([a], [a])
splitCount (min, max) as = do
  let count = fromIntegral $ length as
  when (count < min) $
    decodeThrow DecodeError_CountTooFew{minCount = min, actualCount = count}
  case fromIntegral <$> max of
    Just n -> pure (take n as, drop n as)
    Nothing -> pure (as, [])

count_v2 :: CountSpec -> WithCountSpec decoder -> decoder
count_v2 (min, max) k
  -- TODO: throw DecodeError
  | Just max' <- max, max' < min = error "max must be at least min"
  | otherwise = k.run (min, max)

atLeast_v2 :: Natural -> WithCountSpec (Decoder_v2 a [b]) -> Decoder_v2 a [b]
atLeast_v2 n = count_v2 (n, Nothing)

atMost_v2 :: Natural -> WithCountSpec (Decoder_v2 a [b]) -> Decoder_v2 a [b]
atMost_v2 n = count_v2 (0, Just n)

many_v2 :: WithCountSpec (Decoder_v2 a [b]) -> Decoder_v2 a [b]
many_v2 = atLeast_v2 0

some_v2 :: WithCountSpec (Decoder_v2 a [b]) -> Decoder_v2 a [b]
some_v2 = atLeast_v2 1

optional_v2 :: WithCountSpec (Decoder_v2 a [b]) -> Decoder_v2 a (Maybe b)
optional_v2 = fmap listToMaybe . count_v2 (0, Just 1)

setDefault_v2 :: b -> Decoder_v2 a (Maybe b) -> Decoder_v2 a b
setDefault_v2 def = fmap (fromMaybe def)

required_v2 :: WithCountSpec (Decoder_v2 a [b]) -> Decoder_v2 a b
required_v2 = fmap expectOne . count_v2 (1, Just 1)
  where
    expectOne = \case
      [x] -> x
      _ -> error "required_v2: unexpected result from count_v2"

document_v2 :: NodesDecoder_v2 a -> DocumentDecoder_v2 a
document_v2 = validateNodes

node_v2 :: Identifier -> NodeDecoder_v2 a -> WithCountSpec (NodesDecoder_v2 [a])
node_v2 name decoder0 = WithCountSpec $ \countSpec ->
  DecodeArrow_v2
    [MapItemSchema_ExpectKey name $ mkListSchema countSpec decoder.schema]
    (run countSpec)
  where
    decoder = validateNode decoder0
    run countSpec (allNodes, ()) = addContext (ContextNode name) $ do
      let (candidateNodes, remainingNodes) = partition ((== name) . nodeName) allNodes
      (nodes, leftoverNodes) <- splitCount countSpec candidateNodes
      results <-
        forM (zip nodes [0..]) $ \(node, i) ->
          addContext (ContextIndex i) $ runDecoder_v2 decoder node
      pure (leftoverNodes <> remainingNodes, results)

-- TODO: remainingNodes_v2 :: NodeDecoder_v2 a -> NodesDecoder_v2 [a]

children_v2 :: NodesDecoder_v2 a -> NodeDecoder_v2 a
children_v2 decoder =
  DecodeArrow_v2 mempty{childSchemas = decoder.schema} $ \(node, ()) -> do
    (_, result) <- decoder.run (nodeChildren node, ())
    pure (node{obj = node.obj{children = []}}, result)

dashChildren_v2 :: Identifier -> NodeDecoder_v2 a -> NodesDecoder_v2 [a]
dashChildren_v2 name decoder =
  setDefault_v2 [] . optional_v2 . node_v2 name $
    children_v2 . many_v2 . node_v2 "-" $
      decoder

argAt_v2 :: DecodeValue_v2 a => Identifier -> WithCountSpec (NodesDecoder_v2 [a])
argAt_v2 name = WithCountSpec $ \countSpec ->
  setDefault_v2 [] $ optional_v2 $ node_v2 name $ arg_v2.run countSpec

arg_v2 :: DecodeValue_v2 a => WithCountSpec (NodeDecoder_v2 [a])
arg_v2 = WithCountSpec $ \countSpec ->
  DecodeArrow_v2
    mempty{argSchemas = [mkListSchema countSpec decoder.schema]}
    (run countSpec)
  where
    decoder = value_v2
    run countSpec (node, ()) = addContext ContextArgs $ do
      (args, remainingArgs) <- splitCount countSpec $ nodeArgs node
      results <-
        forM (zip args [0..]) $ \(arg, i) ->
          addContext (ContextIndex i) $ runDecoder_v2 decoder arg
      pure (node{obj = node.obj{args = remainingArgs}}, results)

-- TODO: remainingArgs_v2 :: DecodeValue_v2 a => NodeDecoder_v2 [a]

-- TODO: prop_v2 :: DecodeValue_v2 a => Identifier -> NodeDecoder_v2 a
-- TODO: remainingProps_v2 :: DecodeValue_v2 a => NodeDecoder_v2 (Map Identifier a)

value_v2 :: forall a. DecodeValue_v2 a => ValueDecoder_v2 a
value_v2 = DecodeArrow_v2 schema $ \(annValue, ()) ->
  case annValue of
    Ann{ann = Just givenAnn} | Just validAnns <- mValidAnns, givenAnn `notElem` validAnns ->
      decodeThrow DecodeError_MismatchedAnn{givenAnn = givenAnn, validAnns = validAnns}
    Ann{obj = value} ->
      (<|> decodeThrow DecodeError_AnnValueDecodeFail{typeHint = typeHint, annValue = annValue}) $ do
        (annValue,) <$> runDecoder_v2 decoder value
  where
    decoder@(DecodeArrow_v2 validSchemas _) = valueDecoder_v2 @a
    typeHint = typeRep (Proxy @a)
    mValidAnns = maybeList $ validTypeAnns_v2 (Proxy @a)
    schema =
      ValueSchema
        { typeHint = Just typeHint
        , validAnns = mValidAnns
        , validSchemas = maybeList validSchemas
        }
    maybeList xs = if null xs then Nothing else Just xs

baseValueDecoder_v2 :: [ValueTypeSchema] -> (Value -> DecodeM_v2 a) -> BaseValueDecoder_v2 a
baseValueDecoder_v2 tys f = DecodeArrow_v2 (Set.fromList tys) $ \(value, ()) -> (value,) <$> f value

any_v2 :: BaseValueDecoder_v2 Value
any_v2 = baseValueDecoder_v2 [minBound .. maxBound] pure

text_v2 :: BaseValueDecoder_v2 Text
text_v2 = baseValueDecoder_v2 [TextSchema] $ \case
  Text s -> pure s
  v -> decodeThrow DecodeError_ValueDecodeFail{expectedType = "text", value = v}

number_v2 :: BaseValueDecoder_v2 Scientific
number_v2 = baseValueDecoder_v2 [NumberSchema] $ \case
  Number x -> pure x
  v -> decodeThrow DecodeError_ValueDecodeFail{expectedType = "number", value = v}

bool_v2 :: BaseValueDecoder_v2 Bool
bool_v2 = baseValueDecoder_v2 [BoolSchema] $ \case
  Bool x -> pure x
  v -> decodeThrow DecodeError_ValueDecodeFail{expectedType = "bool", value = v}

null_v2 :: BaseValueDecoder_v2 ()
null_v2 = baseValueDecoder_v2 [NullSchema] $ \case
  Null -> pure ()
  v -> decodeThrow DecodeError_ValueDecodeFail{expectedType = "null", value = v}

oneOf_v2 :: [BaseValueDecoder_v2 a] -> BaseValueDecoder_v2 a
oneOf_v2 = \case
  [] -> empty
  d : ds -> d <|> oneOf_v2 ds

class Typeable a => DecodeValue_v2 a where
  validTypeAnns_v2 :: Proxy a -> [Identifier]
  validTypeAnns_v2 _ = []
  valueDecoder_v2 :: BaseValueDecoder_v2 a
instance DecodeValue_v2 Value where
  valueDecoder_v2 = any_v2
instance DecodeValue_v2 Text where
  validTypeAnns_v2 _ = ["string", "text"]
  valueDecoder_v2 = text_v2
-- TODO: Add Word8, Int8, ...
instance DecodeValue_v2 Integer where
  validTypeAnns_v2 _ = ["i8", "i16", "i32", "i64", "i128", "u8", "u16", "u32", "u64", "u128", "isize", "usize"]
  valueDecoder_v2 = toInteger <$> valueDecoder_v2 @Int64
instance DecodeValue_v2 Int64 where
  validTypeAnns_v2 _ = ["i64"]
  valueDecoder_v2 = withDecoder_v2 number_v2 $ \x -> do
    unless (Scientific.isInteger x) $
      fail_v2 $ "Expected integer, got: " <> (Text.pack . show) x
    maybe (fail_v2 $ "Number is too large: " <> (Text.pack . show) x) pure $
      Scientific.toBoundedInteger @Int64 x
-- TODO: Add Double, Float, Rational
instance DecodeValue_v2 Scientific where
  validTypeAnns_v2 _ = ["f32", "f64", "decimal64", "decimal128"]
  valueDecoder_v2 = number_v2
instance DecodeValue_v2 Bool where
  validTypeAnns_v2 _ = ["bool", "boolean"]
  valueDecoder_v2 = bool_v2
instance DecodeValue_v2 a => DecodeValue_v2 (Maybe a) where
  validTypeAnns_v2 _ = validTypeAnns_v2 (Proxy @a)
  valueDecoder_v2 = oneOf_v2 [Nothing <$ null_v2, Just <$> valueDecoder_v2]
