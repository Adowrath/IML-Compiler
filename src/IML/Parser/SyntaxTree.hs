module IML.Parser.SyntaxTree where

-- | This just represents all rules that do not correspond to data definitions.
-- progParamList ::=
--     LPAREN [progParam {COMMA progParam}] RPAREN
-- paramList ::=
--     LPAREN [param {COMMA param}] RPAREN
-- exprList ::=
--     LPAREN [expr {COMMA expr}] RPAREN
--
-- globImps ::=
--     globImp {COMMA globImp}
-- exprs ::=
--     expr {COMMA expr}
-- idents ::=
--     IDENT {COMMA IDENT}
--
-- cpsCmd ::=
--     cmd {SEMICOLON cmd}
-- cpsDecl ::=
--     decl {SEMICOLON decl}
-- cpsStoDecl ::=
--     stoDecl {SEMICOLON stoDecl}
--
-- globInits ::=
--      INIT idents
data CatchAll

-- | program ::=
--       PROGRAM IDENT progParamList
--       [GLOBAL cpsDecl] DO cpsCmd ENDPROGRAM
-- NOTE: The declaration types are separated already.
data Program =
  Program Ident [ProgParam] [StoreDeclaration] [FunctionDeclaration] [ProcedureDeclaration] [Command]
  deriving (Eq, Show)

-- | decl ::=
--       stoDecl
--     | funDecl
--     | procDecl
data Declaration
  = SDecl StoreDeclaration
  | FDecl FunctionDeclaration
  | PDecl ProcedureDeclaration
  deriving (Eq, Show)

-- | stoDecl ::=
--       [CHANGEMODE] typedIdent
data StoreDeclaration =
  StoreDeclaration (Maybe ChangeMode) TypedIdentifier
  deriving (Eq, Show)

-- | funDecl ::=
--       FUN IDENT paramList
--       RETURNS stoDecl
--       [GLOBAL globImps]
--       [LOCAL cpsStoDecl] DO cpsCmd ENDFUN
data FunctionDeclaration =
  FunctionDeclaration Ident [Param] StoreDeclaration [GlobalImport] [StoreDeclaration] [Command]
  deriving (Eq, Show)

-- | procDecl ::=
--       PROC IDENT paramList
--       [GLOBAL globImps]
--       [LOCAL cpsStoDecl] DO cpsCmd ENDPROC
data ProcedureDeclaration =
  ProcedureDeclaration Ident [Param] [GlobalImport] [StoreDeclaration] [Command]
  deriving (Eq, Show)

-- | globImp ::=
--       [FLOWMODE] [CHANGEMODE] IDENT
data GlobalImport =
  GlobalImport (Maybe FlowMode) (Maybe ChangeMode) Ident
  deriving (Eq, Show)

-- | progParam ::=
--       [FLOWMODE] [CHANGEMODE] typedIdent
data ProgParam =
  ProgParam (Maybe FlowMode) (Maybe ChangeMode) TypedIdentifier
  deriving (Eq, Show)

-- | param ::=
--       [FLOWMODE] [MECHMODE] [CHANGEMODE] typedIdent
data Param =
  Param (Maybe FlowMode) (Maybe MechMode) (Maybe ChangeMode) TypedIdentifier
  deriving (Eq, Show)

-- | typedIdent ::=
--       IDENT COLON ATOMTYPE
data TypedIdentifier =
  TypedIdentifier Ident AtomicType
  deriving (Eq, Show)

-- | cmd ::=
--       SKIP
--     | exprs BECOMES exprs
--     | IF expr THEN cpsCmd ELSE cpsCmd ENDIF
--     | WHILE expr DO cpsCmd ENDWHILE
--     | CALL IDENT exprList [globInits]
--     | DEBUGIN expr
--     | DEBUGOUT expr
data Command
  = SkipCommand
  | AssignCommand [Expr] [Expr]
  | IfCommand Expr [Command] [Command]
  | WhileCommand Expr [Command]
  | CallCommand Ident [Expr] [Ident] -- Global inits = list of identifiers
  | DebugInCommand Expr
  | DebugOutCommand Expr
  deriving (Eq, Show)

-- | expr  ::=
--       term1 [CONDOPR expr COLON expr]
--   term1 ::=
--       term2 [BOOLOPR term1]
--   term2 ::=
--       term3 [RELOPR term3]
--   term3 ::=
--       term4 term3'
--   term3' ::=
--       ADDOPR term4 term3'
--     | Epsilon
--   term4 ::=
--       factor
--     | term4 MULTOPR factor
--   factor ::=
--       LITERAL
--     | IDENT [INIT | exprList]
--     | monadicOpr factor
--     | LPAREN expr RPAREN
--   monadicOpr ::=
--       NOT
--     | ADDOPR
data Expr
  = LiteralExpr AtomicType Literal
  | FunctionCallExpr AtomicType Ident [Expr]
  | NameExpr AtomicType Ident Bool -- Variable names, bool = init or not
  | UnaryExpr AtomicType UnaryOpr Expr
  | BinaryExpr AtomicType BinaryOpr Expr Expr
  | ConditionalExpr AtomicType Expr Expr Expr
  deriving (Eq, Show)

data ChangeMode
  = ConstChange
  | VarChange
  deriving (Eq, Show)

data FlowMode
  = InFlow
  | OutFlow
  | InOutFlow
  deriving (Eq, Show)

data MechMode
  = CopyMech
  | RefMech
  deriving (Eq, Show)

data AtomicType
  = BoolType
  | Int64Type
  | Untyped
  deriving (Eq, Show)

data Literal
  = BoolLiteral Bool
  | Int64Literal Integer
  deriving (Eq, Show)

data UnaryOpr
  = Not
  | UnaryPlus
  | UnaryMinus
  deriving (Eq, Show)

data BinaryOpr
  = MultOpr
  | DivEOpr
  | DivFOpr
  | DivTOpr
  | ModEOpr
  | ModFOpr
  | ModTOpr
  | PlusOpr
  | MinusOpr
  | LTOpr
  | GTOpr
  | LTEOpr
  | GTEOpr
  | EqOpr
  | NeqOpr
  | CAndOpr
  | COrOpr
  deriving (Eq, Show)

type Ident = String
