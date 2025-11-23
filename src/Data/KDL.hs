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

  -- * TODO: categorize
  renderAnn,
  renderAnnValue,
  renderValue,

  -- * Node list helpers
  findNode,
  findNodes,
  getArgAt,
  getArgsAt,
  getDashChildren,

  -- * Decode
  fail,
  lift,
  withDecoder,
  decodeWith,
  decodeFileWith,
  count,
  atLeast,
  atMost,
  many,
  some,
  optional,
  setDefault,
  required,
  document,
  node,
  children,
  dashChildren,
  argAt,
  arg,
  value,
  any,
  text,
  number,
  bool,
  null,
  oneOf,
  DecodeValue (..),
  renderDecodeError,
  DecodeM,
  DecodeError,
  DecodeArrow,
  Decoder,
  DocumentDecoder,
  NodesDecoder,
  NodeDecoder,
  ValueDecoder,
  BaseValueDecoder,

  -- * Low-level parse
  parse,
  parseFile,
) where

import Control.Applicative (Alternative (..))
import Control.Arrow (Arrow (..), ArrowChoice (..), (>>>))
import Control.Category qualified as Category
import Control.Monad (forM, unless, when, (>=>))
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

data DecodeM a = DecodeM (forall r. (Context -> BaseDecodeError -> r) -> (a -> r) -> r)
data DecodeError = DecodeError Context BaseDecodeError
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

instance Functor DecodeM where
  fmap f (DecodeM k) = DecodeM $ \onFail onSuccess -> k onFail (onSuccess . f)
instance Applicative DecodeM where
  pure x = DecodeM $ \_ onSuccess -> onSuccess x
  DecodeM kf <*> DecodeM ka = DecodeM $ \onFail onSuccess ->
    kf onFail $ \f -> ka onFail (onSuccess . f)
instance Monad DecodeM where
  DecodeM ka >>= k = DecodeM $ \onFail onSuccess ->
    ka onFail $ \a -> let DecodeM kb = k a in kb onFail onSuccess
instance Alternative DecodeM where
  empty = fail "<empty>"
  DecodeM k1 <|> DecodeM k2 = DecodeM $ \onFail onSuccess -> k1 (\_ _ -> k2 onFail onSuccess) onSuccess

decodeThrow :: BaseDecodeError -> DecodeM a
decodeThrow e = DecodeM $ \onFail _ -> onFail [] e

fail :: Text -> DecodeM a
fail = decodeThrow . DecodeError_Custom

runDecodeM :: DecodeM a -> Either DecodeError a
runDecodeM (DecodeM f) = f (\ctx msg -> Left $ DecodeError ctx msg) Right

renderDecodeError :: DecodeError -> Text
renderDecodeError (DecodeError ctx e) = "At: " <> (Text.pack . show) ctx <> "\n" <> (Text.pack . show) e -- TODO: render pretty

data DecodeArrow s i a b = DecodeArrow{schema :: s, run :: (i, a) -> DecodeM (i, b)}
type Decoder a b = DecodeArrow (SchemaOf a) a () b
type ValidatedDecoder a b = DecodeArrow (Validated (SchemaOf a)) a () b
type DocumentDecoder a = ValidatedDecoder [AnnNode] a
type NodesDecoder a = Decoder [AnnNode] a
type NodeDecoder a = Decoder AnnNode a
type ValueDecoder a = Decoder AnnValue a
type BaseValueDecoder a = Decoder Value a

type family SchemaOf a where
  SchemaOf [AnnNode] = NodesSchema
  SchemaOf AnnNode = NodeSchema
  SchemaOf AnnValue = ValueSchema
  SchemaOf Value = BaseValueSchema
  SchemaOf (Validated a) = Validated (SchemaOf a)

instance Monoid s => Category.Category (DecodeArrow s i) where
  id = lift pure
  DecodeArrow s1 bc . DecodeArrow s2 ab = DecodeArrow (s1 <> s2) $ ab >=> bc
instance Monoid s => Arrow (DecodeArrow s i) where
  arr f = lift (pure . f)
  DecodeArrow s1 bc *** DecodeArrow s2 bc' =
    DecodeArrow (s1 <> s2) $ \(i0, (b, b')) -> do
      (i1, c) <- bc (i0, b)
      (i2, c') <- bc' (i1, b')
      pure (i2, (c, c'))
instance Monoid s => ArrowChoice (DecodeArrow s i) where
  DecodeArrow s1 bc +++ DecodeArrow s2 bc' =
    DecodeArrow (s1 <> s2) $ \case
      (i, Left b) -> (fmap . fmap) Left $ bc (i, b)
      (i, Right b') -> (fmap . fmap) Right $ bc' (i, b')
instance Functor (DecodeArrow s i a) where
  fmap f (DecodeArrow schema run) = DecodeArrow schema $ ((fmap . fmap) f . run)
instance Monoid s => Applicative (DecodeArrow s i a) where
  pure = arr . const
  DecodeArrow s1 kf <*> DecodeArrow s2 kx =
    DecodeArrow (s1 <> s2) $ \(i0, a) -> do
      (i1, f) <- kf (i0, a)
      (i2, x) <- kx (i1, a)
      pure (i2, f x)
instance Monoid s => Alternative (DecodeArrow s i a) where
  empty = lift $ \_ -> empty
  DecodeArrow s1 run1 <|> DecodeArrow s2 run2 = DecodeArrow (s1 <> s2) $ \x -> run1 x <|> run2 x

lift :: Monoid s => (a -> DecodeM b) -> DecodeArrow s i a b
lift run = DecodeArrow mempty $ \(i, a) -> (i,) <$> run a

withDecoder :: Monoid s => DecodeArrow s i a b -> (b -> DecodeM c) -> DecodeArrow s i a c
withDecoder decoder f = decoder >>> lift f

type Context = [ContextItem]
data ContextItem
  = ContextNode Identifier
  | ContextArgs
  | ContextIndex Natural
  | ContextProp Identifier
  deriving (Show, Eq)

addContext :: ContextItem -> DecodeM a -> DecodeM a
addContext ctxItem (DecodeM f) = DecodeM $ \onFail onSuccess -> f (onFail . (ctxItem :)) onSuccess

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

decodeWith :: DocumentDecoder a -> Text -> Either DecodeError a
decodeWith decoder = decodeFromParseResult decoder . parse

decodeFileWith :: DocumentDecoder a -> FilePath -> IO (Either DecodeError a)
decodeFileWith decoder = fmap (decodeFromParseResult decoder) . parseFile

decodeFromParseResult :: DocumentDecoder a -> Either Text Document -> Either DecodeError a
decodeFromParseResult decoder = runDecodeM . \case
  Left e -> decodeThrow $ DecodeError_ParseError e
  Right doc -> runDecoder decoder doc

runDecoder :: DecodeArrow s a () b -> a -> DecodeM b
runDecoder decoder a = snd <$> decoder.run (a, ())

newtype Validated a = Validated {unwrap :: a}
  deriving (Show, Eq, Semigroup, Monoid)

validateNodes :: Decoder [AnnNode] a -> ValidatedDecoder [AnnNode] a
validateNodes decoder =
  DecodeArrow (Validated decoder.schema) $ \(nodes, ()) -> do
    (nodes', result) <- decoder.run (nodes, ())
    checkAllNodesDecoded decoder.schema nodes'
    pure (nodes', result)

checkAllNodesDecoded :: NodesSchema -> [AnnNode] -> DecodeM ()
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

validateNode :: Decoder AnnNode a -> ValidatedDecoder AnnNode a
validateNode decoder =
  DecodeArrow (Validated decoder.schema) $ \(node, ()) -> do
    (node', result) <- decoder.run (node, ())
    checkFullNodeDecoded decoder.schema node'
    pure (node', result)

checkFullNodeDecoded :: NodeSchema -> AnnNode -> DecodeM ()
checkFullNodeDecoded schema node = do
  unless (null node.obj.args) $
    decodeThrow (DecodeError_UnusedArgs node.obj.args)
  -- TODO: check props
  checkAllNodesDecoded schema.childSchemas node.obj.children

type CountSpec = (Natural, Maybe Natural)
newtype WithCountSpec decoder = WithCountSpec{run :: CountSpec -> decoder}

mkListSchema :: CountSpec -> s -> ListSchema s
mkListSchema (min, max) itemSchema = ListSchema{..}

splitCount :: CountSpec -> [a] -> DecodeM ([a], [a])
splitCount (min, max) as = do
  let count = fromIntegral $ length as
  when (count < min) $
    decodeThrow DecodeError_CountTooFew{minCount = min, actualCount = count}
  case fromIntegral <$> max of
    Just n -> pure (take n as, drop n as)
    Nothing -> pure (as, [])

count :: CountSpec -> WithCountSpec decoder -> decoder
count (min, max) k
  -- TODO: throw DecodeError
  | Just max' <- max, max' < min = error "max must be at least min"
  | otherwise = k.run (min, max)

atLeast :: Natural -> WithCountSpec (Decoder a [b]) -> Decoder a [b]
atLeast n = count (n, Nothing)

atMost :: Natural -> WithCountSpec (Decoder a [b]) -> Decoder a [b]
atMost n = count (0, Just n)

many :: WithCountSpec (Decoder a [b]) -> Decoder a [b]
many = atLeast 0

some :: WithCountSpec (Decoder a [b]) -> Decoder a [b]
some = atLeast 1

optional :: WithCountSpec (Decoder a [b]) -> Decoder a (Maybe b)
optional = fmap listToMaybe . count (0, Just 1)

setDefault :: b -> Decoder a (Maybe b) -> Decoder a b
setDefault def = fmap (fromMaybe def)

required :: WithCountSpec (Decoder a [b]) -> Decoder a b
required = fmap expectOne . count (1, Just 1)
  where
    expectOne = \case
      [x] -> x
      _ -> error "required: unexpected result from count"

document :: NodesDecoder a -> DocumentDecoder a
document = validateNodes

node :: Identifier -> NodeDecoder a -> WithCountSpec (NodesDecoder [a])
node name decoder0 = WithCountSpec $ \countSpec ->
  DecodeArrow
    [MapItemSchema_ExpectKey name $ mkListSchema countSpec decoder.schema]
    (run countSpec)
  where
    decoder = validateNode decoder0
    run countSpec (allNodes, ()) = addContext (ContextNode name) $ do
      let (candidateNodes, remainingNodes) = partition ((== name) . nodeName) allNodes
      (nodes, leftoverNodes) <- splitCount countSpec candidateNodes
      results <-
        forM (zip nodes [0..]) $ \(node, i) ->
          addContext (ContextIndex i) $ runDecoder decoder node
      pure (leftoverNodes <> remainingNodes, results)

-- TODO: remainingNodes :: NodeDecoder a -> NodesDecoder [a]

children :: NodesDecoder a -> NodeDecoder a
children decoder =
  DecodeArrow mempty{childSchemas = decoder.schema} $ \(node, ()) -> do
    (_, result) <- decoder.run (node.obj.children, ())
    pure (node{obj = node.obj{children = []}}, result)

dashChildren :: Identifier -> NodeDecoder a -> NodesDecoder [a]
dashChildren name decoder =
  setDefault [] . optional . node name $
    children . many . node "-" $
      decoder

argAt :: DecodeValue a => Identifier -> WithCountSpec (NodesDecoder [a])
argAt name = WithCountSpec $ \countSpec ->
  setDefault [] $ optional $ node name $ arg.run countSpec

arg :: DecodeValue a => WithCountSpec (NodeDecoder [a])
arg = WithCountSpec $ \countSpec ->
  DecodeArrow
    mempty{argSchemas = [mkListSchema countSpec decoder.schema]}
    (run countSpec)
  where
    decoder = value
    run countSpec (node, ()) = addContext ContextArgs $ do
      (args, remainingArgs) <- splitCount countSpec node.obj.args
      results <-
        forM (zip args [0..]) $ \(arg, i) ->
          addContext (ContextIndex i) $ runDecoder decoder arg
      pure (node{obj = node.obj{args = remainingArgs}}, results)

-- TODO: remainingArgs :: DecodeValue a => NodeDecoder [a]

-- TODO: prop :: DecodeValue a => Identifier -> NodeDecoder a
-- TODO: remainingProps :: DecodeValue a => NodeDecoder (Map Identifier a)

value :: forall a. DecodeValue a => ValueDecoder a
value = DecodeArrow schema $ \(annValue, ()) ->
  case annValue of
    Ann{ann = Just givenAnn} | Just validAnns <- mValidAnns, givenAnn `notElem` validAnns ->
      decodeThrow DecodeError_MismatchedAnn{givenAnn = givenAnn, validAnns = validAnns}
    Ann{obj = value} ->
      (<|> decodeThrow DecodeError_AnnValueDecodeFail{typeHint = typeHint, annValue = annValue}) $ do
        (annValue,) <$> runDecoder decoder value
  where
    decoder@(DecodeArrow validSchemas _) = valueDecoder @a
    typeHint = typeRep (Proxy @a)
    mValidAnns = maybeList $ validTypeAnns (Proxy @a)
    schema =
      ValueSchema
        { typeHint = Just typeHint
        , validAnns = mValidAnns
        , validSchemas = maybeList validSchemas
        }
    maybeList xs = if null xs then Nothing else Just xs

baseValueDecoder :: [ValueTypeSchema] -> (Value -> DecodeM a) -> BaseValueDecoder a
baseValueDecoder tys f = DecodeArrow (Set.fromList tys) $ \(value, ()) -> (value,) <$> f value

any :: BaseValueDecoder Value
any = baseValueDecoder [minBound .. maxBound] pure

text :: BaseValueDecoder Text
text = baseValueDecoder [TextSchema] $ \case
  Text s -> pure s
  v -> decodeThrow DecodeError_ValueDecodeFail{expectedType = "text", value = v}

number :: BaseValueDecoder Scientific
number = baseValueDecoder [NumberSchema] $ \case
  Number x -> pure x
  v -> decodeThrow DecodeError_ValueDecodeFail{expectedType = "number", value = v}

bool :: BaseValueDecoder Bool
bool = baseValueDecoder [BoolSchema] $ \case
  Bool x -> pure x
  v -> decodeThrow DecodeError_ValueDecodeFail{expectedType = "bool", value = v}

null :: BaseValueDecoder ()
null = baseValueDecoder [NullSchema] $ \case
  Null -> pure ()
  v -> decodeThrow DecodeError_ValueDecodeFail{expectedType = "null", value = v}

oneOf :: [BaseValueDecoder a] -> BaseValueDecoder a
oneOf = \case
  [] -> empty
  d : ds -> d <|> oneOf ds

class Typeable a => DecodeValue a where
  validTypeAnns :: Proxy a -> [Identifier]
  validTypeAnns _ = []
  valueDecoder :: BaseValueDecoder a
instance DecodeValue Value where
  valueDecoder = any
instance DecodeValue Text where
  validTypeAnns _ = ["string", "text"]
  valueDecoder = text
-- TODO: Add Word8, Int8, ...
instance DecodeValue Integer where
  validTypeAnns _ = ["i8", "i16", "i32", "i64", "i128", "u8", "u16", "u32", "u64", "u128", "isize", "usize"]
  valueDecoder = toInteger <$> valueDecoder @Int64
instance DecodeValue Int64 where
  validTypeAnns _ = ["i64"]
  valueDecoder = withDecoder number $ \x -> do
    unless (Scientific.isInteger x) $
      fail $ "Expected integer, got: " <> (Text.pack . show) x
    maybe (fail $ "Number is too large: " <> (Text.pack . show) x) pure $
      Scientific.toBoundedInteger @Int64 x
-- TODO: Add Double, Float, Rational
instance DecodeValue Scientific where
  validTypeAnns _ = ["f32", "f64", "decimal64", "decimal128"]
  valueDecoder = number
instance DecodeValue Bool where
  validTypeAnns _ = ["bool", "boolean"]
  valueDecoder = bool
instance DecodeValue a => DecodeValue (Maybe a) where
  validTypeAnns _ = validTypeAnns (Proxy @a)
  valueDecoder = oneOf [Nothing <$ null, Just <$> valueDecoder]
