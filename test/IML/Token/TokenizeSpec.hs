module IML.Token.TokenizeSpec (spec) where

import Test.Hspec
import IML.Token.Tokenizer
import IML.Token.Tokens

spec :: Spec
spec =
  describe "the tokenizer" $
    mapM_ tokenizeTest testCases

tokenizeTest :: (String, String, TokenList) -> SpecWith ()
tokenizeTest (name, text, expected) =
  it ("should properly tokenize " ++ name) $
      tokenize text `shouldBe` expected

testCases :: [(String, String, TokenList)]
testCases = [
    ("an empty string", "", []),
    ("a whitespace-string", " \f\n", []),
    ("simple tokens", "(),;::=", [LParen, RParen, Comma, Semicolon, Colon, Becomes]),
    ("arithmetic operators",
        "+-*",
        Operator <$> [Plus, Minus, Times]),
    ("relational operators",
        ">>=<<==/=",
        Operator <$> [GreaterThan, GreaterThanEquals, LessThan, LessThanEquals, Equals, NotEquals]),
    -- ("logical operators",
    --     "/\\\\/",
    --     Operator <$> [And, Or, CAnd, COr]),
    ("the bool type", "bool", [Type BoolType]),
    -- TODO: Rest
    ("an identifier", "d1mD1_ding’", [Ident String])
  ]
