module IML.Parser.ParserSpec
  ( spec
  ) where

import           IML.Parser.GeneralParser (parse)
import           IML.Parser.Parser
import qualified IML.Parser.SyntaxTree    as ST
import qualified IML.Token.Tokens         as T
import           Test.Hspec

minusOp :: T.Token
minusOp = T.Operator T.Minus

tryParse :: Parser a -> [T.Token] -> a
tryParse parser tokens =
  case parse parser tokens of
    [(res, [])] -> res
    _           -> error "Parsing failed."

-- while x36 <= 67 do
--   x := x-1
-- endwhile
whileLoopExample :: [T.Token]
whileLoopExample =
  [ T.While
  , T.Ident "x36"
  , T.Operator T.LessThanEquals
  , T.IntLit 67
  , T.Do
  , T.Ident "x"
  , T.Becomes
  , T.Ident "x"
  , T.Operator T.Minus
  , T.IntLit 1
  , T.Endwhile
  ]

spec :: Spec
spec =
  describe "the parser" $ do
    it "should correctly parse a-b" $
      tryParse parseExpr [T.Ident "a", minusOp, T.Ident "b"] `shouldBe`
      ST.BinaryExpr ST.MinusOpr (ST.NameExpr "a" False) (ST.NameExpr "b" False)
    it "should correctly parse a-b-c as (a-b)-c" $
      tryParse parseExpr [T.Ident "a", minusOp, T.Ident "b", minusOp, T.Ident "c"] `shouldBe`
      ST.BinaryExpr
        ST.MinusOpr
        (ST.BinaryExpr ST.MinusOpr (ST.NameExpr "a" False) (ST.NameExpr "b" False))
        (ST.NameExpr "c" False)
    it "should parse a simple while loop" $
      tryParse parseCmd whileLoopExample `shouldBe`
      ST.WhileCommand
        (ST.BinaryExpr ST.LTEOpr (ST.NameExpr "x36" False) (ST.LiteralExpr $ ST.Int64Literal 67))
        [ ST.AssignCommand
            [ST.NameExpr "x" False]
            [ST.BinaryExpr ST.MinusOpr (ST.NameExpr "x" False) (ST.LiteralExpr $ ST.Int64Literal 1)]
        ]
