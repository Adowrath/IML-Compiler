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
    -- TODO: Rest
    ("an identifier", "d1mD1_ding'", [Ident "d1mD1_ding'"]),
    ("a simple while loop",
        unlines [
          "while  x36 <=37",
          "  x := x + 1",
          "endwhile"
        ],
        [While, Ident "x36", Operator LessThanEquals, IntLit 37,
          Ident "x", Becomes, Ident "x", Operator Plus, IntLit 1,
          Endwhile])
  ]
