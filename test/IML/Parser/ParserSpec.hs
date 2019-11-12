module IML.Parser.ParserSpec (spec) where

import Test.Hspec
import IML.Parser.Parser

spec :: Spec
spec =
  describe "the parser" $ do
    it "should parse adc" $
      parseProgram [TA, TD, TC] `shouldBe` Right (A1 B)
    it "should parse b" $
      parseProgram [TB] `shouldBe` Right A2
    it "shouldn't parse c" $
      parseProgram [TC] `shouldBe` Left "Failed parsing A."
