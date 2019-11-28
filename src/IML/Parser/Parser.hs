module IML.Parser.Parser where

{-
import IML.Token.Tokens (Literal, OpType, Token(Ident, Init))

type Start = Expr
type ParseResult r = Either String (r, TokenList)

parseProgram :: [Terminal] -> Either String Start
parseProgram xs = do
  (a, xss) <- parseExpr xs
  if not $ null xss
    then Left "We failed to parse the entire input."
    else Right a

-- expr ::= term1 {BOOLOPR term1}
data Expr = Expr Term1 [(OpType, Term1)]
-- term1 ::= term2 [RELOPR term2]
data Term1 = Term1 Term2 (Maybe (OpType, Term2))
-- term2 ::= term3 {ADDOPR term3}
data Term2 = Term2 Term3 [(OpType, Term3)]
-- term3 ::= factor {MULTOPR factor}
data Term3 = Term3 Factor [(OpType, Factor)]
-- factor ::= LITERAL
--            | IDENT [INIT | exprList]
--            | monadicOpr factor
--            | LPAREN expr RPAREN
data Factor = Factor1 Literal
            | Factor2 Ident (Maybe Init)
            | Factor3 Ident ExprList
            | Factor4 MonadicOpr Factor
            | Factor5 Expr
-- exprList ::= LPAREN [expr {COMMA expr}] RPAREN
data ExprList = ExprList Maybe (Expr, [Expr])
-- monadicOpr ::= NOTOPR | ADDOPR
data MonadicOpr = NotOpr | AddOpr
-}


type Start = A
type ParseResult r = Either String (r, [Terminal])

data Terminal = TA | TB | TC | TD
data A = A1 B | A2 deriving (Eq, Show)
data B = B         deriving (Eq, Show)

parseProgram :: [Terminal] -> Either String Start
parseProgram xs = do
  (a, xss) <- parseA xs
  if not $ null xss
    then Left "We failed to parse the entire input."
    else Right a

parseA :: [Terminal] -> ParseResult A
parseA (TA : xs) = do
   (b, xss) <- parseB xs
   case xss of
     (TC : xsss) -> Right (A1 b, xsss)
     _           -> Left "Expected c."
parseA (TB : xs) = Right (A2, xs)
parseA _ = Left "Failed parsing A."

parseB :: [Terminal] -> ParseResult B
parseB (TD : xs) = Right (B, xs)
parseB _ = Left "Failed parsing B."
