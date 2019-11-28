module IML.Parser.Parser where

import           IML.Parser.GeneralParser
import           IML.Token.Tokens         (Token)

type Parser a = GenParser Token a

token :: Token -> Parser Token
token = terminal

--token :: Token -> Parser Token
--token tok = item >>=
{-
program ::=
    PROGRAM IDENT progParamList
    [GLOBAL cpsDecl] DO cpsCmd ENDPROGRAM

decl ::=
      stoDecl
    | funDecl
    | procDecl
stoDecl ::=
      [CHANGEMODE] typedIdent
funDecl ::=
      FUN IDENT paramList
      RETURNS stoDecl
      [GLOBAL globImps]
      [LOCAL cpsStoDecl] DO cpsCmd ENDFUN
procDecl ::=
      PROC IDENT paramList
      [GLOBAL globImps]
      [LOCAL cpsStoDecl] DO cpsCmd ENDPROC
globImps ::=
      globImp {COMMA globImp}
globImp ::=
      [FLOWMODE] [CHANGEMODE] IDENT
cpsDecl ::=
      decl {SEMICOLON decl}
cpsStoDecl ::=
      stoDecl {SEMICOLON stoDecl}

progParamList ::=
      LPAREN [progParam {COMMA progParam}] RPAREN
progParam ::=
      [FLOWMODE] [CHANGEMODE] typedIdent
paramList ::=
      LPAREN [param {COMMA param}] RPAREN
param ::=
      [FLOWMODE] [MECHMODE] [CHANGEMODE] typedIdent
typedIdent ::=
      IDENT COLON ATOMTYPE

cmd ::=
      SKIP
    | exprs BECOMES exprs
    | IF expr THEN cpsCmd ELSE cpsCmd ENDIF
    | WHILE expr DO cpsCmd ENDWHILE
    | CALL IDENT exprList [globInits]
    | DEBUGIN expr
    | DEBUGOUT expr
exprs ::=
      expr {COMMA expr}
cpsCmd ::=
      cmd {SEMICOLON cmd}
globInits ::=
      INIT idents
idents ::=
      IDENT {COMMA IDENT}

expr  ::=
      term1 [CONDOPR expr COLON expr]
term1 ::=
      term2 [BOOLOPR term1]
term2 ::=
      term3 [RELOPR term3]
term3 ::=
      term4 term3'
term3' ::=
      ADDOPR term4 term3'
    | Epsilon
term4 ::=
      factor
    | term4 MULTOPR factor
factor ::=
      LITERAL
    | IDENT [INIT | exprList]
    | monadicOpr factor
    | LPAREN expr RPAREN
exprList ::=
      LPAREN [expr {COMMA expr}] RPAREN
monadicOpr ::=
      NOT
    | ADDOPR
-}
{-
-}
type Start = A

type ParseResult r = Either String (r, [Terminal])

data Terminal
  = TA
  | TB
  | TC
  | TD

data A
  = A1 B
  | A2
  deriving (Eq, Show)

data B =
  B
  deriving (Eq, Show)

parseProgram :: [Terminal] -> Either String Start
parseProgram xs = do
  (a, xss) <- parseA xs
  if not $ null xss
    then Left "We failed to parse the entire input."
    else Right a

parseA :: [Terminal] -> ParseResult A
parseA (TA:xs) = do
  (b, xss) <- parseB xs
  case xss of
    (TC:xsss) -> Right (A1 b, xsss)
    _         -> Left "Expected c."
parseA (TB:xs) = Right (A2, xs)
parseA _ = Left "Failed parsing A."

parseB :: [Terminal] -> ParseResult B
parseB (TD:xs) = Right (B, xs)
parseB _       = Left "Failed parsing B."
