{-# LANGUAGE OverloadedStrings #-}

module Hooky.ConfigSpec (spec) where

import Hooky.Config (
  matchesGlob,
  toGlob,
 )
import Skeletest

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
