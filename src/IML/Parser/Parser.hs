module IML.Parser.Parser where

import           IML.Parser.GeneralParser
import qualified IML.Parser.SyntaxTree    as Syntax
import           IML.Token.Tokens         as Tokens (Token)
import qualified IML.Token.Tokens         as Tokens

type Parser a = GenParser Token a

token :: Token -> Parser Token
token = terminal

commaList :: Parser a -> Parser [a]
commaList = someSep Tokens.Comma

semiList :: Parser a -> Parser [a]
semiList = someSep Tokens.Semicolon

-- | program ::=
--       PROGRAM IDENT progParamList
--       [GLOBAL cpsDecl] DO cpsCmd ENDPROGRAM
parseProgram :: Parser Syntax.Program
parseProgram = undefined

-- | decl ::=
--         stoDecl
--       | funDecl
--       | procDecl
parseDecl :: Parser Syntax.Declaration
parseDecl = undefined

-- | stoDecl ::=
--         [CHANGEMODE] typedIdent
parseStoDecl :: Parser Syntax.StoreDeclaration
parseStoDecl = undefined

-- | funDecl ::=
--         FUN IDENT paramList
--         RETURNS stoDecl
--         [GLOBAL globImps]
--         [LOCAL cpsStoDecl] DO cpsCmd ENDFUN
parseFunDecl :: Parser Syntax.FunctionDeclaration
parseFunDecl = undefined

-- | procDecl ::=
--         PROC IDENT paramList
--         [GLOBAL globImps]
--         [LOCAL cpsStoDecl] DO cpsCmd ENDPROC
parseProcDecl :: Parser Syntax.ProcedureDeclaration
parseProcDecl = undefined

-- | globImps ::=
--         globImp {COMMA globImp}
parseGlobImps :: Parser [Syntax.GlobalImport]
parseGlobImps = undefined

-- | globImp ::=
--         [FLOWMODE] [CHANGEMODE] IDENT
parseGlobImp :: Parser Syntax.GlobalImport
parseGlobImp = undefined

-- | cpsDecl ::=
--         decl {SEMICOLON decl}
parseCpsDecl :: Parser [Syntax.Declaration]
parseCpsDecl = undefined

-- | cpsStoDecl ::=
--         stoDecl {SEMICOLON stoDecl}
parseCpsStoDecl :: Parser [Syntax.StoreDeclaration]
parseCpsStoDecl = undefined

-- | progParamList ::=
--         LPAREN [progParam {COMMA progParam}] RPAREN
parseProgParamList :: Parser [Syntax.ProgParam]
parseProgParamList = undefined

-- | progParam ::=
--         [FLOWMODE] [CHANGEMODE] typedIdent
parseProgParam :: Parser Syntax.ProgParam
parseProgParam = undefined

-- | paramList ::=
--         LPAREN [param {COMMA param}] RPAREN
parseParamList :: Parser [Syntax.Param]
parseParamList = undefined

-- | param ::=
--         [FLOWMODE] [MECHMODE] [CHANGEMODE] typedIdent
parseParam :: Parser Syntax.Param
parseParam = undefined

-- | typedIdent ::=
--         IDENT COLON ATOMTYPE
parseTypedIdent :: Parser Syntax.TypedIdentifier
parseTypedIdent = undefined

-- | cmd ::=
--         SKIP
--       | exprs BECOMES exprs
--       | IF expr THEN cpsCmd ELSE cpsCmd ENDIF
--       | WHILE expr DO cpsCmd ENDWHILE
--       | CALL IDENT exprList [globInits]
--       | DEBUGIN expr
--       | DEBUGOUT expr
parseCmd :: Parser Syntax.Command
parseCmd = undefined

-- | exprs ::=
--         expr {COMMA expr}
parseExprs :: Parser [Syntax.Expr]
parseExprs = undefined

-- | cpsCmd ::=
--         cmd {SEMICOLON cmd}
parseCpsCmd :: Parser [Syntax.Command]
parseCpsCmd = undefined

-- | globInits ::=
--         INIT idents
parseGlobInits :: Parser [Syntax.Ident]
parseGlobInits = undefined

-- | idents ::=
--         IDENT {COMMA IDENT}
parseIdents :: Parser [Syntax.Ident]
parseIdents = undefined

-- | expr  ::=
--         term1 [CONDOPR expr COLON expr]
parseExpr :: Parser Syntax.Expr
parseExpr = undefined

-- | term1 ::=
--         term2 [BOOLOPR term1]
parseTerm1 :: Parser Syntax.Expr
parseTerm1 = undefined

-- | term2 ::=
--         term3 [RELOPR term3]
parseTerm2 :: Parser Syntax.Expr
parseTerm2 = undefined

-- | term3 ::=
--         term4 term3'
--   term3' ::=
--         ADDOPR term4 term3'
--       | Epsilon
-- Equivalent to:
--   term3 ::= term4 {ADDOPR term4}
parseTerm3 :: Parser Syntax.Expr
parseTerm3 = undefined

-- | term4 ::=
--         factor
--       | term4 MULTOPR factor
parseTerm4 :: Parser Syntax.Expr
parseTerm4 = undefined

-- | factor ::=
--         LITERAL
--       | IDENT [INIT | exprList]
--       | monadicOpr factor
--       | LPAREN expr RPAREN
parseFactor :: Parser Syntax.Expr
parseFactor = undefined

-- | exprList ::=
--         LPAREN [expr {COMMA expr}] RPAREN
parseExprList :: Parser [Syntax.Expr]
parseExprList = undefined

-- | monadicOpr ::=
--         NOT
--       | ADDOPR
parseMonadicOpr :: Parser Syntax.UnaryOpr
parseMonadicOpr = undefined
