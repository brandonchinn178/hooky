{-# LANGUAGE OverloadedStrings #-}

module Hooky.ConfigSpec (spec) where

import Data.Text (Text)
import Data.Text qualified as Text
import Hooky.Config (
  matchesGlob,
  matchesGlobs,
  parseRepoConfig,
  toGlob,
 )
import Skeletest
import Skeletest.Predicate qualified as P
import Skeletest.Prop qualified as Prop
import Skeletest.Prop.Gen qualified as Gen
import Skeletest.Prop.Range qualified as Range

spec :: Spec
spec = do
  lintRulesSpec
  globSpec

lintRulesSpec :: Spec
lintRulesSpec = do
  describe "lint_rules" $ do
    describe "check_broken_symlinks" $ do
      supportsFiles "check_broken_symlinks"

    describe "check_case_conflict" $ do
      supportsFiles "check_case_conflict"

    describe "check_merge_conflict" $ do
      supportsFiles "check_merge_conflict"

    describe "end_of_file_fixer" $ do
      supportsFiles "end_of_file_fixer"

    describe "no_commit_to_branch" $ do
      doesntSupportFiles "no_commit_to_branch"

    describe "trailing_whitespace" $ do
      supportsFiles "trailing_whitespace"
 where
  supportsFiles rule = do
    it "supports 'files' configuration" $ do
      parseRepoConfig ("lint_rules { - " <> rule <> " { files *.txt; }; }")
        `shouldSatisfy` P.right P.anything

  doesntSupportFiles rule = do
    it "fails if 'files' specified" $ do
      parseRepoConfig ("lint_rules { - " <> rule <> " { files *.txt; }; }")
        `shouldSatisfy` P.left (P.hasInfix "'files' config is not supported for")

globSpec :: Spec
globSpec = do
  describe "Glob" $ do
    describe "matchesGlob" $ do
      let
        glob `matches` input = \expected ->
          it (show glob <> " matches " <> show input <> " ==> " <> show expected) $ do
            matchesGlob (toGlob glob) input `shouldBe` expected
        (==>) = ($)

      "*" `matches` "Foo.hs" ==> True
      "*.hs" `matches` "Foo.hs" ==> True
      "*.hs" `matches` "Foo.hs.txt" ==> False
      "*.hs" `matches` "Foo/Bar.hs" ==> True
      "*.hs" `matches` "Foo/Bar.txt" ==> False
      "*.hs" `matches` "Foo/Bar.hs/Baz" ==> False
      "Foo*.hs" `matches` "Foo.hs" ==> True
      "Foo*.hs" `matches` "FooBar.hs" ==> True
      "Foo*.hs" `matches` "FooBar.hs.txt" ==> False
      "!*.hs" `matches` "Foo.hs" ==> False
      "!*.hs" `matches` "Foo/Bar.hs" ==> False
      "!*.hs" `matches` "Foo/Bar.txt" ==> True
      "!*.hs" `matches` "Foo/Bar.hs/Baz" ==> True
      "/*.hs" `matches` "Foo.hs" ==> True
      "/*.hs" `matches` "Foo/Bar.hs" ==> False
      "Foo/Bar.hs" `matches` "Foo/Bar.hs" ==> True
      "Foo/Bar.hs" `matches` "path/to/Foo/Bar.hs" ==> True
      "/Foo/Bar.hs" `matches` "Foo/Bar.hs" ==> True
      "/Foo/Bar.hs" `matches` "path/to/Foo/Bar.hs" ==> False
      "**/*.hs" `matches` "Foo.hs" ==> True
      "**/*.hs" `matches` "Foo/Bar.hs" ==> True
      "**/*.hs" `matches` "Foo/Bar/Baz.hs" ==> True
      "**/*.hs" `matches` "Foo.txt" ==> False
      "**/*.hs" `matches` "Foo/Bar/Baz.txt" ==> False
      "!**/*.hs" `matches` "Foo.hs" ==> False
      "!**/*.hs" `matches` "Foo/Bar.hs" ==> False
      "!**/*.hs" `matches` "Foo/Bar/Baz.hs" ==> False
      "!**/*.hs" `matches` "Foo.txt" ==> True
      "!**/*.hs" `matches` "Foo/Bar/Baz.txt" ==> True

      prop "'<path>' and '**/<path>' are equivalent" $ do
        Prop.setTestLimit 10000
        g <- forAll genRelGlob
        let g1 = toGlob g
            g2 = toGlob ("**/" <> g)
        (matchesGlob g1 P.=== matchesGlob g2) `shouldSatisfy` P.isoWith genRelPath

    describe "matchesGlobs" $ do
      let
        globs `matches` input = \expected ->
          it (show globs <> " matches " <> show input <> " ==> " <> show expected) $ do
            matchesGlobs (map toGlob globs) input `shouldBe` expected
        (==>) = ($)

      ["**/*"] `matches` "Foo.hs" ==> True
      ["!**/*.golden"] `matches` "Foo.hs" ==> True
      ["!**/*.golden", "**/*"] `matches` "Foo.hs" ==> True

      pure () -- TODO: property test where matchesGlobs [g] === matchesGlob g
 where
  genRelPathLike :: Gen Text -> Gen Text
  genRelPathLike g = Text.intercalate "/" <$> Gen.list (Range.linear 1 10) g

  genRelPath :: Gen Text
  genRelPath = genRelPathLike $ Gen.text (Range.linear 1 30) Gen.unicode

  genRelGlob :: Gen Text
  genRelGlob =
    genRelPathLike $
      fmap Text.concat . Gen.list (Range.linear 1 5) $
        Gen.frequency
          [ (9, Gen.text (Range.linear 1 5) Gen.unicode)
          , (1, pure "*")
          ]
