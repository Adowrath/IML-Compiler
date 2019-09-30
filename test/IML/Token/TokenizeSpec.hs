module IML.Token.TokenizeSpec (spec) where

import Control.Exception (evaluate)
import Test.Hspec
import IML.Token.Tokenizer
import IML.Token.Tokens

spec :: Spec
spec =
  describe "the tokenizer" $ do
    mapM_ tokenizeTest testCases
    it "should throw on tabs" $
      evaluate (tokenize "\t") `shouldThrow` errorCall "We don't like tabs. Actually, we do, but eh."
    it "should throw on integer literals ending in ticks" $
      evaluate (length $ tokenize "12''") `shouldThrow` errorCall "Tick at end of integer literal."

tokenizeTest :: (String, String, TokenList) -> SpecWith ()
tokenizeTest (name, text, expected) =
  it ("should properly tokenize " ++ name) $
      tokenize text `shouldBe` expected

testCases :: [(String, String, TokenList)]
testCases = [
    ("an empty string", "", []),
    ("a whitespace-string", " \n", []),
    ("simple tokens", "( ) , ; : :=", [LParen, RParen, Comma, Semicolon, Colon, Becomes]),
    ("arithmetic operators",
        "+ - *",
        Operator <$> [Plus, Minus, Times]),
    ("relational operators",
        "> >= < <= = /=",
        Operator <$> [GreaterThan, GreaterThanEquals, LessThan, LessThanEquals, Equals, NotEquals]),
    ("logical operators",
        "/\\? \\/?",
        Operator <$> [CAnd, COr]),
    ("a few integer literals",
        "12345 1'2''3'''4''''5",
        [IntLit 12345, IntLit 12345]),
    ("the bool type", "bool", [Type BoolType]),
    ("an identifier", "d1mD1_ding'", [Ident "d1mD1_ding'"]),
    ("a simple while loop",
        unlines [
          "while x36  <=67 do",
          "     x := x-1",
          "  endwhile"
        ],
        [While, Ident "x36", Operator LessThanEquals, IntLit 67, Do,
          Ident "x", Becomes, Ident "x", Operator Minus, IntLit 1,
          Endwhile])
  ]
