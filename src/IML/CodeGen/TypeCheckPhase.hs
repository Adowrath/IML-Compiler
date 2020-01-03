{-# LANGUAGE ImplicitParams #-}

module IML.CodeGen.TypeCheckPhase where

import           Control.Applicative   ((<|>))
import           Data.List             (find)
import           Data.Maybe            (fromMaybe)
import qualified IML.Parser.SyntaxTree as S

-- | Types the given program completely.
typeCheckProgram :: S.Program -> S.Program
typeCheckProgram (S.Program pName pParams stores funcs procs pCmds) =
  let ?context = TypingContext -- Construct a global context.
        { _contextParams = progParamToParam <$> pParams
        , _contextLocals = stores
        , _contextFunctions = funcs
        , _contextProcedures = procs
        }
   in S.Program pName pParams stores (typeCheckFunction <$> funcs) (typeCheckProcedure <$> procs) (typeCheckBlock pCmds)

-- | Types the given function according to the supplied global context.
typeCheckFunction :: (?context :: TypingContext) => S.FunctionDeclaration -> S.FunctionDeclaration
typeCheckFunction (S.FunctionDeclaration name params retDecl globImps locals body) =
  S.FunctionDeclaration name params retDecl globImps locals newBody
  where
    newBody =
      let ?context = ?context -- Copy of the context, but new params and locals
            {_contextParams = params ++ (globalImportToParam <$> globImps), _contextLocals = retDecl : locals}
       in typeCheckBlock body

-- | Types the given procedure according to the supplied global context.
typeCheckProcedure :: (?context :: TypingContext) => S.ProcedureDeclaration -> S.ProcedureDeclaration
typeCheckProcedure (S.ProcedureDeclaration name params globImps locals body) =
  S.ProcedureDeclaration name params globImps locals newBody
  where
    newBody =
      let ?context = ?context -- Copy of the context, but new params and locals
            {_contextParams = params ++ (globalImportToParam <$> globImps), _contextLocals = locals}
       in typeCheckBlock body

--                  --
-- Command typings. --
--                  --
-- | Types the given block according to the supplied context.
typeCheckBlock :: (?context :: TypingContext) => [S.Command] -> [S.Command]
typeCheckBlock cmds = typeCheckCommand <$> cmds

-- | Types a given command in the supplied context.
typeCheckCommand :: (?context :: TypingContext) => S.Command -> S.Command
typeCheckCommand cmd =
  case cmd of
    S.SkipCommand -> S.SkipCommand
    S.AssignCommand leftExprs rightExprs -> typeCheckAssignment leftExprs rightExprs
    S.IfCommand condition thenBlock elseBlock -> typeCheckIf condition thenBlock elseBlock
    S.WhileCommand condition doBlock -> typeCheckWhile condition doBlock
    S.CallCommand procName procParams globInits -> typeCheckCall procName procParams globInits
    S.DebugInCommand expr -> S.DebugInCommand $ typeCheckExpr expr
    S.DebugOutCommand expr -> S.DebugOutCommand $ typeCheckExpr expr

-- | Type checks an assignment command.
typeCheckAssignment :: (?context :: TypingContext) => [S.Expr] -> [S.Expr] -> S.Command
typeCheckAssignment [] [] = error "Assignment with empty sides."
typeCheckAssignment leftExprs rightExprs = S.AssignCommand leftTyped rightTyped
  where
    (leftTyped, rightTyped) = checkAssignability leftExprs rightExprs
    checkAssignability [] [] = ([], [])
    checkAssignability [] _ = error "Too many expressions on right hand side of assignment."
    checkAssignability _ [] = error "Not enough expressions on right hand side of assignment."
    checkAssignability (l:ls) (r:rs) =
      if check
        then (l' : ls', r' : rs')
        else error $ "Cannot assign " ++ show l' ++ " to " ++ show r'
      where
        check = isLExpr l' && (getType l' == getType r')
        l' = typeCheckExpr l
        r' = typeCheckExpr r
        (ls', rs') = checkAssignability ls rs

-- | Type checks an if command.
typeCheckIf :: (?context :: TypingContext) => S.Expr -> [S.Command] -> [S.Command] -> S.Command
typeCheckIf condition thenBlock elseBlock = S.IfCommand typedCondition thenTyped elseTyped
  where
    typedCondition = typeCheckExprConforming condition S.BoolType
    thenTyped = typeCheckBlock thenBlock
    elseTyped = typeCheckBlock elseBlock

-- | Type checks a while command.
typeCheckWhile :: (?context :: TypingContext) => S.Expr -> [S.Command] -> S.Command
typeCheckWhile condition doBlock = S.WhileCommand typedCondition doTyped
  where
    typedCondition = typeCheckExprConforming condition S.BoolType
    doTyped = typeCheckBlock doBlock

-- | Type checks a call command.
typeCheckCall :: (?context :: TypingContext) => S.Ident -> [S.Expr] -> [S.Ident] -> S.Command
typeCheckCall procName procParms globInits = error "TODO: Does not check call commands." -- TODO

--                     --
-- Expression typings. --
--                     --
-- | Type checks an expression in the given typing context.
typeCheckExpr :: (?context :: TypingContext) => S.Expr -> S.Expr
typeCheckExpr expr =
  case expr of
    S.LiteralExpr tp lit -> typeCheckLiteral tp lit
    S.FunctionCallExpr tp name params -> typeCheckFunctionCall tp name params
    S.NameExpr tp name isInit -> typeCheckName tp name isInit
    S.UnaryExpr tp opr subExpr -> typeCheckUnary tp opr subExpr
    S.BinaryExpr tp opr leftExpr rightExpr -> typeCheckBinary tp opr leftExpr rightExpr
    S.ConditionalExpr tp condition trueExpr falseExpr -> typeCheckConditional tp condition trueExpr falseExpr

-- | Type checks a literal expression.
typeCheckLiteral :: (?context :: TypingContext) => S.AtomicType -> S.Literal -> S.Expr
typeCheckLiteral S.Untyped lit@(S.BoolLiteral _) = S.LiteralExpr S.BoolType lit
typeCheckLiteral S.Untyped lit@(S.Int64Literal _) = S.LiteralExpr S.Int64Type lit
typeCheckLiteral _ _ = error "Cannot retype a literal."

-- | Type checks a x.
typeCheckFunctionCall :: (?context :: TypingContext) => S.AtomicType -> S.Ident -> [S.Expr] -> S.Expr
typeCheckFunctionCall _ name params = undefined

-- | Type checks a variable name in the given context.
typeCheckName :: (?context :: TypingContext) => S.AtomicType -> S.Ident -> Bool -> S.Expr
typeCheckName _ name = S.NameExpr (lookupVariableType name) name

-- | Type checks a unary expression.
typeCheckUnary :: (?context :: TypingContext) => S.AtomicType -> S.UnaryOpr -> S.Expr -> S.Expr
typeCheckUnary _ opr subExpr = S.UnaryExpr newType opr typedExpr
  where
    newType =
      case (opr, getType typedExpr) of
        (S.Not, S.BoolType) -> S.BoolType
        (S.UnaryMinus, S.Int64Type) -> S.Int64Type
        (S.UnaryPlus, S.Int64Type) -> S.Int64Type
        (_, exprType) ->
          error $ "Expression of type " ++ show exprType ++ " could not be applied to operator: " ++ show opr
    typedExpr = typeCheckExpr subExpr

-- | Type checks a binary expression.
typeCheckBinary :: (?context :: TypingContext) => S.AtomicType -> S.BinaryOpr -> S.Expr -> S.Expr -> S.Expr
typeCheckBinary _ opr leftExpr rightExpr = S.BinaryExpr newType opr typedLeft typedRight
  where
    newType =
      case (getType typedLeft, opr, getType typedRight) of
        (S.Int64Type, S.MinusOpr, S.Int64Type) -> S.Int64Type
        (S.Int64Type, S.MultOpr, S.Int64Type) -> S.Int64Type
        (S.Int64Type, S.DivEOpr, S.Int64Type) -> S.Int64Type
        (S.Int64Type, S.ModEOpr, S.Int64Type) -> S.Int64Type
        (S.Int64Type, S.PlusOpr, S.Int64Type) -> S.Int64Type
        (S.Int64Type, S.LTEOpr, S.Int64Type) -> S.BoolType
        (S.Int64Type, S.GTEOpr, S.Int64Type) -> S.BoolType
        (S.Int64Type, S.LTOpr, S.Int64Type) -> S.BoolType
        (S.Int64Type, S.GTOpr, S.Int64Type) -> S.BoolType
        (lt, S.EqOpr, rt)
          | lt == rt -> S.BoolType
        (lt, S.NeqOpr, rt)
          | lt == rt -> S.BoolType
        (S.BoolType, S.CAndOpr, S.BoolType) -> S.BoolType
        (S.BoolType, S.COrOpr, S.BoolType) -> S.BoolType
        (leftType, _, rightType) ->
          error $
          "Operator " ++ show opr ++ " could not match operands: " ++ show leftType ++ " and " ++ show rightType ++ "."
    typedLeft = typeCheckExpr leftExpr
    typedRight = typeCheckExpr rightExpr

-- | Type checks a conditional expression.
typeCheckConditional :: (?context :: TypingContext) => S.AtomicType -> S.Expr -> S.Expr -> S.Expr -> S.Expr
typeCheckConditional _ condition trueExpr falseExpr = S.ConditionalExpr newType typedCondition typedTrue typedFalse
  where
    newType =
      case (getType typedTrue, getType typedFalse) of
        (trueType, falseType)
          | trueType == falseType -> trueType
          | otherwise ->
            error $
            "Conditional expression has unequal types in true [" ++
            show trueType ++ "] and false [" ++ show falseType ++ "] side."
    typedCondition = typeCheckExprConforming condition S.BoolType
    typedTrue = typeCheckExpr trueExpr
    typedFalse = typeCheckExpr falseExpr

--                      --
-- Type check utilities --
--                      --
-- | A typing context for the current location in the program.
-- Every procedure and function has a typing context that is
-- partially derived from the global context that applies to
-- the program's do block.
data TypingContext =
  TypingContext
    { _contextParams     :: [S.Param]
    , _contextLocals     :: [S.StoreDeclaration]
    , _contextFunctions  :: [S.FunctionDeclaration]
    , _contextProcedures :: [S.ProcedureDeclaration]
    }
  deriving (Eq, Show)

-- | Helper accessor with implicit context.
contextParams :: (?context :: TypingContext) => [S.Param]
contextParams = _contextParams ?context

-- | Helper accessor with implicit context.
contextLocals :: (?context :: TypingContext) => [S.StoreDeclaration]
contextLocals = _contextLocals ?context

-- | Helper accessor with implicit context.
contextFunctions :: (?context :: TypingContext) => [S.FunctionDeclaration]
contextFunctions = _contextFunctions ?context

-- | Helper accessor with implicit context.
contextProcedures :: (?context :: TypingContext) => [S.ProcedureDeclaration]
contextProcedures = _contextProcedures ?context

-- | Converts the given program parameter into a dummy normal parameter.
-- This is useful to check the global do block.
progParamToParam :: S.ProgParam -> S.Param
progParamToParam (S.ProgParam flowMode changeMode typedIdent) = S.Param flowMode (Just S.CopyMech) changeMode typedIdent

-- | Constructs a dummy ref parameter from the given global import syntax node.
-- TODO: Maybe consider adding typing to the global imports.
globalImportToParam :: (?context :: TypingContext) => S.GlobalImport -> S.Param
globalImportToParam (S.GlobalImport flowMode changeMode name) =
  S.Param flowMode (Just S.RefMech) changeMode typedIdentifier
  where
    typedIdentifier = S.TypedIdentifier name globalType
    globalType =
      case find (\(S.StoreDeclaration _ (S.TypedIdentifier n _)) -> n == name) contextLocals of
        Nothing -> error $ "Can't find global " ++ name
        Just (S.StoreDeclaration _ (S.TypedIdentifier _ t)) -> t

-- | Checks whether the supplied expression is a valid LExpr.
-- Currently, this is true only for NameExprs, naked variables.
isLExpr :: S.Expr -> Bool
isLExpr S.NameExpr {} = True
isLExpr _             = False

-- | Extracts the type from a given expression.
getType :: S.Expr -> S.AtomicType
getType (S.LiteralExpr exprType _)         = exprType
getType (S.FunctionCallExpr exprType _ _)  = exprType
getType (S.NameExpr exprType _ _)          = exprType
getType (S.UnaryExpr exprType _ _)         = exprType
getType (S.BinaryExpr exprType _ _ _)      = exprType
getType (S.ConditionalExpr exprType _ _ _) = exprType

-- | Type checks the given expression and validates it against
-- the expected type. If successful, returns the newly typed expression.
typeCheckExprConforming :: (?context :: TypingContext) => S.Expr -> S.AtomicType -> S.Expr
typeCheckExprConforming expr expectedType =
  if actualType /= expectedType
    then error $ "Expected type " ++ show expectedType ++ ", but was: " ++ show actualType ++ "."
    else typedExpr
  where
    typedExpr = typeCheckExpr expr
    actualType = getType typedExpr

-- | Tries to lookup a function in the given context.
-- Fails with error if not found.
lookupFunction :: (?context :: TypingContext) => S.Ident -> S.FunctionDeclaration
lookupFunction funcName =
  fromMaybe
    (error $ "Function not found: " ++ funcName)
    (find (\(S.FunctionDeclaration n _ _ _ _ _) -> funcName == n) functions)
  where
    functions = contextFunctions

-- | Tries to lookup a procedure in the given context.
-- Fails with error if not found.
lookupProcedure :: (?context :: TypingContext) => S.Ident -> S.ProcedureDeclaration
lookupProcedure procName =
  fromMaybe
    (error $ "Procedure not found: " ++ procName)
    (find (\(S.ProcedureDeclaration n _ _ _ _) -> procName == n) procedures)
  where
    procedures = contextProcedures

-- | Tries to lookup the type of a variable in the given context.
-- Fails with error if not found.
lookupVariableType :: (?context :: TypingContext) => S.Ident -> S.AtomicType
lookupVariableType varName =
  fromMaybe (error $ "Could not find variable named " ++ varName ++ ".") (typeAsParameter <|> typeAsLocal)
  where
    locals = contextLocals
    params = contextParams
    typeAsParameter =
      (\(S.Param _ _ _ (S.TypedIdentifier _ t)) -> t) <$>
      find (\(S.Param _ _ _ (S.TypedIdentifier n _)) -> varName == n) params
    typeAsLocal =
      (\(S.StoreDeclaration _ (S.TypedIdentifier _ t)) -> t) <$>
      find (\(S.StoreDeclaration _ (S.TypedIdentifier n _)) -> varName == n) locals
