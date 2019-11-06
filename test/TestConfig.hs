module Main where

import Data.List
import Test.Hspec

import Config

main :: IO ()
main = hspec $ do
  describe "parseLeaf" $ do
    it "parseLeaf empty string" $
      parse "" `shouldBe` [Text ""]
    it "parseLeaf no var" $
      parse "foobar" `shouldBe` [Text "foobar"]
    it "parseLeaf vars" $
      parse "foo((ab))bar((cd))baz" `shouldBe` [Text "foo", Var "ab", Text "bar", Var "cd", Text "baz"]
    -- ...
 where
  parse = parseLeaf "((" "))"
