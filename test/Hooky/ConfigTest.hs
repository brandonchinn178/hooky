{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Hooky.ConfigTest (test) where

import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Test.Tasty
import Test.Tasty.HUnit

import Hooky.Config
  ( Check (..)
  , CommandDefinition (..)
  , Config (..)
  , Source (..)
  , SourceReference (..)
  , parseConfig
  )

test :: TestTree
test =
  testGroup
    "Hooky.Config"
    [ testParseConfig
    ]

testParseConfig :: TestTree
testParseConfig =
  testGroup
    "parseConfig"
    [ testParseChecks
    , testParseSources
    , testParseExclude
    ]
  where
    getOneCheck = \case
      Config{cfgChecks = [check]} -> check
      config -> error $ "Got unexpected config: " <> show config
    testParseChecks =
      testGroup
        "Config.check[]"
        [ testCase "has a reasonable default" $ do
            let Config{cfgChecks} = parse []
            cfgChecks @?= []
        , testCase "parses a minimal check" $ do
            let check =
                  getOneCheck . parse $
                    [ "[[check]]"
                    , "name = 'always-success'"
                    , "command = 'true'"
                    ]
            check
              @?= Check
                { checkName = "always-success"
                , checkCommand = ExplicitCommand ["true"]
                , checkFiles = []
                }
        , testCase "parses multiple checks" $ do
            let Config{cfgChecks} =
                  parse $
                    [ "[[check]]"
                    , "name = 'test1'"
                    , "command = 'true'"
                    , "[[check]]"
                    , "name = 'test2'"
                    , "command = 'true'"
                    ]
            map checkName cfgChecks @?= ["test1", "test2"]
        , testCase "parses a check source" $ do
            let Check{checkCommand} =
                  getOneCheck . parse $
                    [ "[[check]]"
                    , "name = 'test'"
                    , "source = 'upstream'"
                    ]
            checkCommand @?= CommandFromSource (SourceReference "upstream" "test")
        , testCase "parses a check source with explicit check name" $ do
            let Check{checkCommand} =
                  getOneCheck . parse $
                    [ "[[check]]"
                    , "name = 'my-test'"
                    , "source = { name = 'upstream', check = 'test' }"
                    ]
            checkCommand @?= CommandFromSource (SourceReference "upstream" "test")
        , testCase "errors without command nor source" $ do
            let result =
                  parseEither
                    [ "[[check]]"
                    , "name = 'test'"
                    ]
            case result of
              Left _ -> return ()
              _ -> assertFailure $ "Unexpected result: " ++ show result
        , testCase "parses a single-executable command" $ do
            let Check{checkCommand} =
                  getOneCheck . parse $
                    [ "[[check]]"
                    , "name = 'test'"
                    , "command = 'true'"
                    ]
            checkCommand @?= ExplicitCommand ["true"]
        , testCase "parses a shell command" $ do
            let Check{checkCommand} =
                  getOneCheck . parse $
                    [ "[[check]]"
                    , "name = 'test'"
                    , "command = 'python3 do_check.py'"
                    ]
            checkCommand @?= ExplicitCommand ["/bin/sh", "-c", "python3 do_check.py \"$@\""]
        , testCase "parses a command array" $ do
            let Check{checkCommand} =
                  getOneCheck . parse $
                    [ "[[check]]"
                    , "name = 'test'"
                    , "command = ['python', 'test.py']"
                    ]
            checkCommand @?= ExplicitCommand ["python", "test.py"]
        , testCase "parses a single file filter" $ do
            let Check{checkFiles} =
                  getOneCheck . parse $
                    [ "[[check]]"
                    , "name = 'test'"
                    , "command = 'true'"
                    , "files = '*.txt'"
                    ]
            checkFiles @?= ["*.txt"]
        , testCase "parses multiple file filters" $ do
            let Check{checkFiles} =
                  getOneCheck . parse $
                    [ "[[check]]"
                    , "name = 'test'"
                    , "command = 'true'"
                    , "files = ['*.txt', '*.md']"
                    ]
            checkFiles @?= ["*.txt", "*.md"]
        ]

    getTestSource = \case
      Config{cfgSources}
        | Just source <- Map.lookup "test" cfgSources -> source
      config -> error $ "Got unexpected config: " <> show config
    testParseSources =
      testGroup
        "Config.source.*"
        [ testCase "has a reasonable default" $ do
            let Config{cfgSources} = parse []
            cfgSources @?= Map.empty
        , testCase "parses multiple sources" $ do
            let Config{cfgSources} =
                  parse
                    [ "[source.test1]"
                    , "url = 'https://example.com/checks1.tar.gz'"
                    , "[source.test2]"
                    , "url = 'https://example.com/checks2.tar.gz'"
                    ]
            cfgSources
              @?= Map.fromList
                [ ("test1", TarSource "https://example.com/checks1.tar.gz")
                , ("test2", TarSource "https://example.com/checks2.tar.gz")
                ]
        , testCase "parses a github url" $ do
            let source =
                  getTestSource . parse $
                    [ "[source.test]"
                    , "github = 'google/hooky-checks'"
                    , "rev = 'v1.0.0'"
                    ]
            source @?= GitSource "https://github.com/google/hooky-checks.git" "v1.0.0"
        , testCase "shows helpful error when github repo is not well-specified" $ do
            let result =
                  parseEither $
                    [ "[source.test]"
                    , "github = 'a/b/c'"
                    , "rev = 'v1.0.0'"
                    ]
            result @?= Left "Invalid github repository: a/b/c"
        , testCase "parses a git url" $ do
            let source =
                  getTestSource . parse $
                    [ "[source.test]"
                    , "git = 'https://github.com/google/hooky-checks.git'"
                    , "rev = 'abcdef'"
                    ]
            source @?= GitSource "https://github.com/google/hooky-checks.git" "abcdef"
        , testCase "parses an archive url" $ do
            let source =
                  getTestSource . parse $
                    [ "[source.test]"
                    , "url = 'https://google.com/hooky-checks.tar.gz'"
                    ]
            source @?= TarSource "https://google.com/hooky-checks.tar.gz"
        ]

    testParseExclude =
      testGroup
        "Config.exclude"
        [ testCase "has a reasonable default" $ do
            let Config{cfgExclude} = parse []
            cfgExclude @?= []
        , testCase "parses a single exclude pattern" $ do
            let Config{cfgExclude} =
                  parse
                    [ "exclude = '*.txt'"
                    ]
            cfgExclude @?= ["*.txt"]
        , testCase "parses multiple exclude patterns" $ do
            let Config{cfgExclude} =
                  parse
                    [ "exclude = ['*.txt', '*.md']"
                    ]
            cfgExclude @?= ["*.txt", "*.md"]
        ]

{-- Helpers --}

parse :: [Text] -> Config
parse = either (error . Text.unpack) id . parseEither

parseEither :: [Text] -> Either Text Config
parseEither = parseConfig . Text.unlines
