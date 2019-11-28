module XmlParseSpec (spec) where

import Config
import Data.Validation
import XmlParse
import System.Exit
import Test.Hspec
import qualified Data.Set as Set

badXml1 = "<foobar></foobar>"
expectedResultForBadXml1 = failWith (UnexpectedTag ["builder"] "foobar" (Just 1))

badXml2 = "<builder></builder>"
expectedResultForBadXml2 = failWith (MissingAttribute "builder" "name")

badXml3 = "<builder name=\"foo\"><foobar/></builder>"
expectedResultForBadXml3 = failWith (UnexpectedTag ["setProperty", "shell"] "foobar" (Just 1))

badXml4 = "<builder name=\"foo\"><shell/></builder>"
expectedResultForBadXml4 = failWith (MissingAttribute "shell" "command")

badXml5 = "<builder name=\"foo\"><setProperty value=\"bar\"/></builder>"
expectedResultForBadXml5 =failWith (MissingAttribute "setProperty" "property")

badXml6 = "<builder name=\"foo\"><setProperty property=\"foo\"/></builder>"
expectedResultForBadXml6 = failWith (MissingAttribute "setProperty" "value")

badXml7 = "<builder><shell/><setProperty/><unknown/></builder>"
expectedResultForBadXml7 =
  Failure $
    Set.fromList
      [ UnexpectedTag ["setProperty", "shell"] "unknown" (Just 1)
      , MissingAttribute "builder" "name"
      , MissingAttribute "shell" "command"
      , MissingAttribute "setProperty" "property"
      , MissingAttribute "setProperty" "value"
      ]

validXml =
  "<builder name=\"ls builder\">\
  \  <shell command=\"ls /\"/>\
  \  <setProperty property=\"prop\" value=\"foobar\"/>\
  \</builder>"
expectedResultForValidXml =
  Success
    [ Builder
        { name = "ls builder"
        , steps =
          [ ShellCmd { cmd = ["ls", "/"] }
          , SetPropertyFromValue { prop = "prop", value = "foobar" }
          ]
        }
    ]

spec :: SpecWith ()
spec =
  describe "parseXmlString" $ do
    it "should succeed on valid XLM" $
      parseXmlString validXml `shouldBe` expectedResultForValidXml
    it "should fail on bad toplevel element" $
      parseXmlString badXml1 `shouldBe` expectedResultForBadXml1
    it "should fail on missing name in builder" $
      parseXmlString badXml2 `shouldBe` expectedResultForBadXml2
    it "should fail on bad step element" $
      parseXmlString badXml3 `shouldBe` expectedResultForBadXml3
    it "should fail on missing command in shell" $
      parseXmlString badXml4 `shouldBe` expectedResultForBadXml4
    it "should fail on missing property in setProperty" $
      parseXmlString badXml5 `shouldBe` expectedResultForBadXml5
    it "should fail on missing value in setProperty" $
      parseXmlString badXml6 `shouldBe` expectedResultForBadXml6
    it "should collect many errors" $
      parseXmlString badXml7 `shouldBe` expectedResultForBadXml7
