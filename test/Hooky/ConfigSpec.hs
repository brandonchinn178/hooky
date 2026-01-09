{-# LANGUAGE OverloadedStrings #-}

module Hooky.ConfigSpec (spec) where

import Data.Text (Text)
import Data.Text qualified as Text
import Hooky.Config (
  matchesGlob,
  matchesGlobs,
  toGlob,
 )
import Skeletest
import Skeletest.Prop.Gen qualified as Gen
import Skeletest.Prop.Range qualified as Range

spec :: Spec
spec = do
  globSpec

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

      -- TODO: This throws GHC error: `internal error: stg_ap_p_ret`
      -- prop "'<path>' and '**/<path>' are equivalent" $ do
      --   g <- forAll genRelGlob
      --   toGlob g `shouldBe` toGlob ("**/" <> g)
      const (pure ()) genRelGlob

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
  genRelGlob :: Gen Text
  genRelGlob =
    fmap (Text.intercalate "/") . Gen.list (Range.linear 1 10) $
      fmap Text.concat . Gen.list (Range.linear 1 5) $
        Gen.frequency
          [ (9, Gen.text (Range.linear 1 5) Gen.unicode)
          , (1, pure "*")
          ]
