{-# LANGUAGE ScopedTypeVariables #-}

module ConfigSpec (spec) where

import Data.List
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Modifiers
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Validation as V

import Config hiding (prop)

testParseVars =
  describe "parseVars" $ do
    it "1" $
      parseVars ("$(", ")") "foo$(b)ar"  `shouldBe` [Left "foo", Right "b", Left "ar"]
    it "no var" $
      parseVars ("$(", ")") "foobar"  `shouldBe` [Left "foobar"]
    it "2" $
      parseVars ("$(", ")") "foo$(b)a$(r)"  `shouldBe` [Left "foo", Right "b", Left "a", Right "r"]
    prop "pretty . parse . pretty = pretty" $
      \(NonEmpty open) (NonEmpty close) chunks ->
          let toStr = pretty open close
              toAst = parseVars (open, close)
          in (toStr. toAst . toStr) chunks === toStr chunks
 where
  pretty open close = concatMap (prettyChunk open close)
  prettyChunk open close (Left str) = str
  prettyChunk open close (Right str) = open ++ str ++ close

testSplitAround :: SpecWith ()
testSplitAround =
  describe "splitAround" $ do
    it "prefix" $
      splitAround "f" "foobar"  `shouldBe` Just ("", "oobar")
    it "middle" $
      splitAround "b" "foobar"  `shouldBe` Just ("foo", "ar")
    it "end" $
      splitAround "r" "foobar"  `shouldBe` Just ("fooba", "")
    it "nothing" $
      splitAround "c" "foobar"  `shouldBe` Nothing

testSplitDelimiters :: SpecWith ()
testSplitDelimiters =
  describe "splitDelimiters" $ do
    it "Nothing" $
      splitDelimiters ("(", ")") "foobar" `shouldBe` Nothing
    it "middle" $
      splitDelimiters ("(", ")") "foo(var)bar" `shouldBe` Just("foo", "var", "bar")
    it "start" $
      splitDelimiters ("(", ")") "(var)bar" `shouldBe` Just("", "var", "bar")
    it "end" $
      splitDelimiters ("(", ")") "foo(var)" `shouldBe` Just("foo", "var", "")
    it "middle2" $
      splitDelimiters ("$(", ")") "foo$(var)" `shouldBe` Just("foo", "var", "")

testSubstitute :: SpecWith ()
testSubstitute = do
  describe "duplicates" $ do
    it "duplicates . duplicates = [] (duplicates doesn't returns duplicates)" $
      property $
        \(x :: [Int]) -> duplicates (duplicates x) `shouldBe` []
    it "duplicates [0, 0] should be [0]" $
      duplicates [0, 0] `shouldBe` [0]
    it "duplicates [0, 0, 0] should be [0]" $
      duplicates [0, 0] `shouldBe` [0]
    it "duplicates [0, 1] should be []" $
      duplicates [0, 1] `shouldBe` []
  describe "subsitute" $ do
    it "should succeed when all variables are known" $
      substitute ("(", ")") goodSubst builder `shouldBe` V.Success expectedSuccess
    it "should fail when some variables are unknown" $
      substitute ("(", ")") badSubst builder `shouldBe` V.Failure expectedErrors
 where
  builder =
    Builder (Just "(a)") "builder"
      [ SetPropertyFromValue "prop" "foo(a)bar(b)baz"
      , ShellCmd (Just "(b)") (Command "ls" ["(a)", "(b)"]) Nothing
      ]
  goodSubst = [("a", "xx"), ("b", "yy")]
  expectedSuccess =
    Builder (Just "xx") "builder"
      [ SetPropertyFromValue "prop" "fooxxbaryybaz"
      , ShellCmd (Just "yy") (Command "ls" ["xx", "yy"]) Nothing
      ]
  badSubst = [("c", "xx")]
  expectedErrors = Set.fromList
    [ KeyNotFound badSubst "a"
    , KeyNotFound badSubst "b"
    ]


spec = do
  testParseVars
  testSplitAround
  testSplitDelimiters
  testSubstitute
