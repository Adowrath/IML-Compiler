module IML.Parser.Parser
(
  module IML.Parser.Parser, 
  parse
) where

import           Control.Applicative
import           Data.Foldable            (foldl')
import           IML.Parser.GeneralParser
import qualified IML.Parser.SyntaxTree    as Syntax
import           IML.Token.Tokens         (Token)
import qualified IML.Token.Tokens         as T

type Parser a = GenParser Token a

token :: Token -> Parser Token
token = terminal

commaList :: Parser a -> Parser [a]
commaList = someSep T.Comma

-- | This parses as LPAREN [parseA {COMMA parseA}] RPAREN.
parenList :: Parser a -> Parser [a]
parenList parseA = token T.LParen *> manySep T.Comma parseA <* token T.RParen

semiList :: Parser a -> Parser [a]
semiList = someSep T.Semicolon

parseIdentifier :: Parser Syntax.Ident
parseIdentifier = do
  tok <- item
  case tok of
    (T.Ident ident) -> return ident
    _               -> empty

orEmpty :: Parser [a] -> Parser [a]
orEmpty parseAs = parseAs <|> pure []

parseLeftRecursive :: Parser Syntax.Expr -> Parser Syntax.BinaryOpr -> Parser Syntax.Expr
parseLeftRecursive nextStep opParser = _refold <$> nextStep <*> _parseTerms
  where
    _refold :: Syntax.Expr -> [(Syntax.BinaryOpr, Syntax.Expr)] -> Syntax.Expr
    _refold = foldl' (uncurry . flip (Syntax.BinaryExpr Syntax.Untyped))
    _parseTerms :: Parser [(Syntax.BinaryOpr, Syntax.Expr)]
    _parseTerms = many ((,) <$> opParser <*> nextStep)

-- Internal helper
parseOperatorToken :: Parser T.OpType
parseOperatorToken = do
  tok <- item
  case tok of
    (T.Operator op) -> return op
    _               -> empty

-- | program ::=
--       PROGRAM IDENT progParamList
--       [GLOBAL cpsDecl] DO cpsCmd ENDPROGRAM
parseProgram :: Parser Syntax.Program
parseProgram =
  Syntax.Program <$> (token T.Program *> parseIdentifier) <*> parseProgParamList <*>
  orEmpty (token T.Global *> parseCpsDecl) <*>
  (token T.Do *> parseCpsCmd <* token T.Endprogram)

-- | decl ::=
--         stoDecl
--       | funDecl
--       | procDecl
parseDecl :: Parser Syntax.Declaration
parseDecl = (Syntax.SDecl <$> parseStoDecl) <|> (Syntax.FDecl <$> parseFunDecl) <|> (Syntax.PDecl <$> parseProcDecl)

-- | stoDecl ::=
--         [CHANGEMODE] typedIdent
parseStoDecl :: Parser Syntax.StoreDeclaration
parseStoDecl = Syntax.StoreDeclaration <$> optional _parseChangeMode <*> parseTypedIdent
  where
    _parseChangeMode = do
      tok <- item
      case tok of
        (T.ChangeMode T.Const) -> return Syntax.ConstChange
        (T.ChangeMode T.Var)   -> return Syntax.VarChange
        _                      -> empty

-- | funDecl ::=
--         FUN IDENT paramList
--         RETURNS stoDecl
--         [GLOBAL globImps]
--         [LOCAL cpsStoDecl] DO cpsCmd ENDFUN
parseFunDecl :: Parser Syntax.FunctionDeclaration
parseFunDecl =
  Syntax.FunctionDeclaration <$> (token T.Fun *> parseIdentifier) <*> parseParamList <*>
  (token T.Returns *> parseStoDecl) <*>
  orEmpty (token T.Global *> parseGlobImps) <*>
  orEmpty (token T.Local *> parseCpsStoDecl) <*>
  (token T.Do *> parseCpsCmd <* token T.Endfun)

-- | procDecl ::=
--         PROC IDENT paramList
--         [GLOBAL globImps]
--         [LOCAL cpsStoDecl] DO cpsCmd ENDPROC
parseProcDecl :: Parser Syntax.ProcedureDeclaration
parseProcDecl =
  Syntax.ProcedureDeclaration <$> (token T.Proc *> parseIdentifier) <*> parseParamList <*>
  orEmpty (token T.Global *> parseGlobImps) <*>
  orEmpty (token T.Local *> parseCpsStoDecl) <*>
  (token T.Do *> parseCpsCmd <* token T.Endproc)

-- | globImps ::=
--         globImp {COMMA globImp}
parseGlobImps :: Parser [Syntax.GlobalImport]
parseGlobImps = commaList parseGlobImp

-- | globImp ::=
--         [FLOWMODE] [CHANGEMODE] IDENT
parseGlobImp :: Parser Syntax.GlobalImport
parseGlobImp = Syntax.GlobalImport <$> optional _parseFlowMode <*> optional _parseChangeMode <*> parseIdentifier
  where
    _parseFlowMode = do
      tok <- item
      case tok of
        (T.FlowMode T.In)    -> return Syntax.InFlow
        (T.FlowMode T.InOut) -> return Syntax.InOutFlow
        (T.FlowMode T.Out)   -> return Syntax.OutFlow
        _                    -> empty
    _parseChangeMode = do
      tok <- item
      case tok of
        (T.ChangeMode T.Const) -> return Syntax.ConstChange
        (T.ChangeMode T.Var)   -> return Syntax.VarChange
        _                      -> empty

-- | cpsDecl ::=
--         decl {SEMICOLON decl}
parseCpsDecl :: Parser [Syntax.Declaration]
parseCpsDecl = semiList parseDecl

-- | cpsStoDecl ::=
--         stoDecl {SEMICOLON stoDecl}
parseCpsStoDecl :: Parser [Syntax.StoreDeclaration]
parseCpsStoDecl = semiList parseStoDecl

-- | progParamList ::=
--         LPAREN [progParam {COMMA progParam}] RPAREN
parseProgParamList :: Parser [Syntax.ProgParam]
parseProgParamList = parenList parseProgParam

-- | progParam ::=
--         [FLOWMODE] [CHANGEMODE] typedIdent
parseProgParam :: Parser Syntax.ProgParam
parseProgParam = Syntax.ProgParam <$> optional _parseFlowMode <*> optional _parseChangeMode <*> parseTypedIdent
  where
    _parseFlowMode = do
      tok <- item
      case tok of
        (T.FlowMode T.In)    -> return Syntax.InFlow
        (T.FlowMode T.InOut) -> return Syntax.InOutFlow
        (T.FlowMode T.Out)   -> return Syntax.OutFlow
        _                    -> empty
    _parseChangeMode = do
      tok <- item
      case tok of
        (T.ChangeMode T.Const) -> return Syntax.ConstChange
        (T.ChangeMode T.Var)   -> return Syntax.VarChange
        _                      -> empty

-- | paramList ::=
--         LPAREN [param {COMMA param}] RPAREN
parseParamList :: Parser [Syntax.Param]
parseParamList = parenList parseParam

-- | param ::=
--         [FLOWMODE] [MECHMODE] [CHANGEMODE] typedIdent
parseParam :: Parser Syntax.Param
parseParam =
  Syntax.Param <$> optional _parseFlowMode <*> optional _parseMechMode <*> optional _parseChangeMode <*> parseTypedIdent
  where
    _parseFlowMode = do
      tok <- item
      case tok of
        (T.FlowMode T.In)    -> return Syntax.InFlow
        (T.FlowMode T.InOut) -> return Syntax.InOutFlow
        (T.FlowMode T.Out)   -> return Syntax.OutFlow
        _                    -> empty
    _parseMechMode = do
      tok <- item
      case tok of
        (T.MechMode T.Ref)  -> return Syntax.RefMech
        (T.MechMode T.Copy) -> return Syntax.CopyMech
        _                   -> empty
    _parseChangeMode = do
      tok <- item
      case tok of
        (T.ChangeMode T.Const) -> return Syntax.ConstChange
        (T.ChangeMode T.Var)   -> return Syntax.VarChange
        _                      -> empty

-- | typedIdent ::=
--         IDENT COLON ATOMTYPE
parseTypedIdent :: Parser Syntax.TypedIdentifier
parseTypedIdent = Syntax.TypedIdentifier <$> parseIdentifier <*> (token T.Colon *> _parseAtomType)
  where
    _parseAtomType = (Syntax.BoolType <$ token (T.Type T.BoolType)) <|> (Syntax.Int64Type <$ token (T.Type T.Int64Type))

-- | cmd ::=
--       SKIP
--     | exprs BECOMES exprs
--     | IF expr THEN cpsCmd ELSE cpsCmd ENDIF
--     | WHILE expr DO cpsCmd ENDWHILE
--     | CALL IDENT exprList [globInits]
--     | DEBUGIN expr
--     | DEBUGOUT expr
parseCmd :: Parser Syntax.Command
parseCmd = _parseSkip <|> _parseBecomes <|> _parseIf <|> _parseWhile <|> _parseCall <|> _parseDebugIn <|> _parseDebugOut
  where
    _parseSkip = Syntax.SkipCommand <$ token T.Skip
    _parseBecomes = Syntax.AssignCommand <$> parseExprs <*> (token T.Becomes *> parseExprs)
    _parseIf =
      Syntax.IfCommand <$> (token T.If *> parseExpr) <*> (token T.Then *> parseCpsCmd) <*>
      (token T.Else *> parseCpsCmd <* token T.Endif)
    _parseWhile =
      Syntax.WhileCommand <$> (token T.While *> parseExpr) <*> (token T.Do *> parseCpsCmd <* token T.Endwhile)
    _parseCall = Syntax.CallCommand <$> (token T.Call *> parseIdentifier) <*> parseExprList <*> orEmpty parseGlobInits
    _parseDebugIn = Syntax.DebugInCommand <$> (token T.DebugIn *> parseExpr)
    _parseDebugOut = Syntax.DebugOutCommand <$> (token T.DebugOut *> parseExpr)

-- | exprs ::=
--         expr {COMMA expr}
parseExprs :: Parser [Syntax.Expr]
parseExprs = commaList parseExpr

-- | cpsCmd ::=
--         cmd {SEMICOLON cmd}
parseCpsCmd :: Parser [Syntax.Command]
parseCpsCmd = semiList parseCmd

-- | globInits ::=
--         INIT idents
parseGlobInits :: Parser [Syntax.Ident]
parseGlobInits = token T.Init *> parseIdents

-- | idents ::=
--         IDENT {COMMA IDENT}
parseIdents :: Parser [Syntax.Ident]
parseIdents = commaList parseIdentifier

-- | expr  ::=
--         term1 [CONDOPR expr COLON expr]
parseExpr :: Parser Syntax.Expr
parseExpr = do
  condition <- parseTerm1
  rest <- optional _parseRest
  return $
    case rest of
      Nothing -> condition
      Just (trueValue, falseValue) -> Syntax.ConditionalExpr Syntax.Untyped condition trueValue falseValue
  where
    _parseRest = (,) <$> (token T.CondOpr *> parseExpr) <*> (token T.Colon *> parseExpr)

-- | term1 ::=
--         term2 [BOOLOPR term1]
parseTerm1 :: Parser Syntax.Expr
parseTerm1 = do
  left <- parseTerm2
  rest <- optional _parseRest
  return $
    case rest of
      Nothing          -> left
      Just (op, right) -> Syntax.BinaryExpr Syntax.Untyped op left right
  where
    _parseRest = do
      opT <- parseOperatorToken
      case opT of
        T.COr  -> (,) <$> pure Syntax.COrOpr <*> parseTerm1
        T.CAnd -> (,) <$> pure Syntax.CAndOpr <*> parseTerm1
        _      -> empty

-- | term2 ::=
--         term3 [RELOPR term3]
parseTerm2 :: Parser Syntax.Expr
parseTerm2 = do
  left <- parseTerm3
  rest <- optional _parseRest
  return $
    case rest of
      Nothing          -> left
      Just (op, right) -> Syntax.BinaryExpr Syntax.Untyped op left right
  where
    _parseRest = do
      opT <- parseOperatorToken
      case opT of
        T.GreaterThan       -> (,) <$> pure Syntax.GTOpr <*> parseTerm3
        T.GreaterThanEquals -> (,) <$> pure Syntax.GTEOpr <*> parseTerm3
        T.LessThan          -> (,) <$> pure Syntax.LTOpr <*> parseTerm3
        T.LessThanEquals    -> (,) <$> pure Syntax.LTEOpr <*> parseTerm3
        T.Equals            -> (,) <$> pure Syntax.EqOpr <*> parseTerm3
        T.NotEquals         -> (,) <$> pure Syntax.NeqOpr <*> parseTerm3
        _                   -> empty

-- | term3 ::=
--         term4 term3'
--   term3' ::=
--         ADDOPR term4 term3'
--       | Epsilon
-- Equivalent to:
--   term3 ::= term4 {ADDOPR term4}
parseTerm3 :: Parser Syntax.Expr
parseTerm3 = parseLeftRecursive parseTerm4 _parseAddOpr
  where
    _parseAddOpr :: Parser Syntax.BinaryOpr
    _parseAddOpr = do
      opT <- parseOperatorToken
      case opT of
        T.Plus  -> return Syntax.PlusOpr
        T.Minus -> return Syntax.MinusOpr
        _       -> empty

-- | term4 ::=
--         term4 term4'
-- | term4' ::=
--         MULTOPR factor term4'
--       | Epsilon
-- | term4 ::=
--         factor {MULTOPR factor}
parseTerm4 :: Parser Syntax.Expr
parseTerm4 = parseLeftRecursive parseFactor _parseMulOpr
  where
    _parseMulOpr :: Parser Syntax.BinaryOpr
    _parseMulOpr = do
      opT <- parseOperatorToken
      case opT of
        T.Times -> return Syntax.MultOpr
        T.DivE  -> return Syntax.DivEOpr
        T.ModE  -> return Syntax.ModEOpr
        _       -> empty

-- | factor ::=
--         LITERAL
--       | IDENT [INIT | exprList]
--       | monadicOpr factor
--       | LPAREN expr RPAREN
parseFactor :: Parser Syntax.Expr
parseFactor = _parseLiteral <|> _parseNameOrCall <|> _parseUnary <|> _parseParens
  where
    _parseLiteral = do
      tok <- item
      case tok of
        (T.BoolLit boolValue) -> return $ Syntax.LiteralExpr Syntax.BoolType $ Syntax.BoolLiteral boolValue
        (T.IntLit int64Value) -> return $ Syntax.LiteralExpr Syntax.Int64Type $ Syntax.Int64Literal int64Value
        _ -> empty
    _parseNameOrCall = do
      ident <- parseIdentifier
      _parseCallList ident <|> _parseWithInit ident <|> pure (Syntax.NameExpr Syntax.Untyped ident False)
      where
        _parseCallList i = Syntax.FunctionCallExpr Syntax.Untyped i <$> parseExprList
        _parseWithInit i = do
          _ <- token T.Init
          return $ Syntax.NameExpr Syntax.Untyped i True
    _parseUnary = Syntax.UnaryExpr Syntax.Untyped <$> parseMonadicOpr <*> parseFactor
    _parseParens = do
      _ <- token T.LParen
      expr <- parseExpr
      _ <- token T.RParen
      return expr

-- | exprList ::=
--         LPAREN [expr {COMMA expr}] RPAREN
parseExprList :: Parser [Syntax.Expr]
parseExprList = parenList parseExpr

-- | monadicOpr ::=
--         NOT
--       | ADDOPR
parseMonadicOpr :: Parser Syntax.UnaryOpr
parseMonadicOpr = do
  op <- parseOperatorToken
  case op of
    T.Not   -> return Syntax.Not
    T.Plus  -> return Syntax.UnaryPlus
    T.Minus -> return Syntax.UnaryMinus
    _       -> empty
