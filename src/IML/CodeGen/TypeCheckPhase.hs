{-# LANGUAGE ImplicitParams #-}

module IML.CodeGen.TypeCheckPhase where

import           Control.Applicative      ((<|>))
import           Data.List                (find)
import           Data.Maybe               (fromMaybe)
import           IML.CodeGen.CompileUtils (Context, buildContext,
                                           buildSubcontext, contextFunctions,
                                           contextLocals, contextParams,
                                           contextProcedures, getType)
import qualified IML.Parser.SyntaxTree    as S
import           Text.Printf

-- | Types the given program completely.
typeCheckProgram :: S.Program -> S.Program
typeCheckProgram (S.Program pName pParams stores funcs procs pCmds) =
  let ?context = buildContext (progParamToParam <$> pParams) stores funcs procs
   in S.Program pName pParams stores (typeCheckFunction <$> funcs) (typeCheckProcedure <$> procs) (typeCheckBlock pCmds)

-- | Types the given function according to the supplied global context.
typeCheckFunction :: (?context :: Context) => S.FunctionDeclaration -> S.FunctionDeclaration
typeCheckFunction (S.FunctionDeclaration name params retDecl globImps locals body) =
  let ?context = buildSubcontext (params ++ (globalImportToParam <$> globImps)) (retDecl : locals)
   in S.FunctionDeclaration name params retDecl globImps locals $ typeCheckBlock body

-- | Types the given procedure according to the supplied global context.
typeCheckProcedure :: (?context :: Context) => S.ProcedureDeclaration -> S.ProcedureDeclaration
typeCheckProcedure (S.ProcedureDeclaration name params globImps locals body) =
  let ?context = buildSubcontext (params ++ (globalImportToParam <$> globImps)) locals
   in S.ProcedureDeclaration name params globImps locals $ typeCheckBlock body

--                  --
-- Command typings. --
--                  --
-- | Types the given block according to the supplied context.
typeCheckBlock :: (?context :: Context) => [S.Command] -> [S.Command]
typeCheckBlock cmds = typeCheckCommand <$> cmds

-- | Types a given command in the supplied context.
typeCheckCommand :: (?context :: Context) => S.Command -> S.Command
typeCheckCommand S.SkipCommand = S.SkipCommand
typeCheckCommand (S.AssignCommand leftExprs rightExprs) = typeCheckAssignment leftExprs rightExprs
typeCheckCommand (S.IfCommand condition thenBlock elseBlock) = typeCheckIf condition thenBlock elseBlock
typeCheckCommand (S.WhileCommand condition doBlock) = typeCheckWhile condition doBlock
typeCheckCommand (S.CallCommand procName procParams globInits) = typeCheckCall procName procParams globInits
typeCheckCommand (S.DebugInCommand expr) =
  if isLExpr typedExpr
    then S.DebugInCommand typedExpr
    else error $ printf "The expression to a debugin command must be an LExpr, but it is not. %s" (show typedExpr)
  where
    typedExpr = typeCheckExpr expr
typeCheckCommand (S.DebugOutCommand expr) = S.DebugOutCommand $ typeCheckExpr expr

-- | Type checks an assignment command.
typeCheckAssignment :: (?context :: Context) => [S.Expr] -> [S.Expr] -> S.Command
typeCheckAssignment [] [] = error "Assignment with empty sides."
typeCheckAssignment leftExprs rightExprs = S.AssignCommand leftTyped rightTyped
  where
    (leftTyped, rightTyped) = checkAssignability leftExprs rightExprs
    checkAssignability [] [] = ([], [])
    checkAssignability [] _ = error "Too many expressions on right hand side of assignment."
    checkAssignability _ [] = error "Not enough expressions on right hand side of assignment."
    checkAssignability (l:ls) (r:rs) =
      if isLExpr l' && (getType l' == getType r')
        then (l' : ls', r' : rs')
        else error $ printf "Cannot assign %s to %s." (show l') (show r')
      where
        l' = typeCheckExpr l
        r' = typeCheckExpr r
        (ls', rs') = checkAssignability ls rs

-- | Type checks an if command.
typeCheckIf :: (?context :: Context) => S.Expr -> [S.Command] -> [S.Command] -> S.Command
typeCheckIf condition thenBlock elseBlock = S.IfCommand typedCondition thenTyped elseTyped
  where
    typedCondition = typeCheckExprConforming condition S.BoolType
    thenTyped = typeCheckBlock thenBlock
    elseTyped = typeCheckBlock elseBlock

-- | Type checks a while command.
typeCheckWhile :: (?context :: Context) => S.Expr -> [S.Command] -> S.Command
typeCheckWhile condition doBlock = S.WhileCommand typedCondition doTyped
  where
    typedCondition = typeCheckExprConforming condition S.BoolType
    doTyped = typeCheckBlock doBlock

-- | Type checks a call command.
typeCheckCall :: (?context :: Context) => S.Ident -> [S.Expr] -> [S.Ident] -> S.Command
typeCheckCall procName paramExprs globInits = S.CallCommand procName typedParams globInits
  where
    typedParams = typeApplication procParams globImports paramExprs globInits
    (S.ProcedureDeclaration _ procParams globImports _ _) = lookupProcedure procName

--                     --
-- Expression typings. --
--                     --
-- | Type checks an expression in the given typing context.
typeCheckExpr :: (?context :: Context) => S.Expr -> S.Expr
typeCheckExpr (S.LiteralExpr tp lit) = typeCheckLiteral tp lit
typeCheckExpr (S.FunctionCallExpr tp name params) = typeCheckFunctionCall tp name params
typeCheckExpr (S.NameExpr tp name isInit) = typeCheckName tp name isInit
typeCheckExpr (S.UnaryExpr tp opr subExpr) = typeCheckUnary tp opr subExpr
typeCheckExpr (S.BinaryExpr tp opr leftExpr rightExpr) = typeCheckBinary tp opr leftExpr rightExpr
typeCheckExpr (S.ConditionalExpr tp condition trueExpr falseExpr) = typeCheckConditional tp condition trueExpr falseExpr

-- | Type checks a literal expression.
typeCheckLiteral :: (?context :: Context) => S.AtomicType -> S.Literal -> S.Expr
typeCheckLiteral S.Untyped lit@(S.BoolLiteral _) = S.LiteralExpr S.BoolType lit
typeCheckLiteral S.Untyped lit@(S.Int64Literal _) = S.LiteralExpr S.Int64Type lit
typeCheckLiteral _ _ = error "Cannot retype a literal."

-- | Type checks a function call.
typeCheckFunctionCall :: (?context :: Context) => S.AtomicType -> S.Ident -> [S.Expr] -> S.Expr
typeCheckFunctionCall _ funcName paramExprs = S.FunctionCallExpr returnType funcName typedParams
  where
    typedParams = typeApplication funcParams [] paramExprs []
    fun@(S.FunctionDeclaration _ funcParams _ _ _ _) = lookupFunction funcName
    returnType = typeOfFunction fun

-- | Type checks a variable name in the given context.
typeCheckName :: (?context :: Context) => S.AtomicType -> S.Ident -> Bool -> S.Expr
typeCheckName _ name = S.NameExpr (lookupVariableType name) name

-- | Type checks a unary expression.
typeCheckUnary :: (?context :: Context) => S.AtomicType -> S.UnaryOpr -> S.Expr -> S.Expr
typeCheckUnary _ opr subExpr = S.UnaryExpr newType opr typedExpr
  where
    newType =
      case (opr, getType typedExpr) of
        (S.Not, S.BoolType) -> S.BoolType
        (S.UnaryMinus, S.Int64Type) -> S.Int64Type
        (S.UnaryPlus, S.Int64Type) -> S.Int64Type
        (_, exprType) ->
          error $ printf "Expression of type %s could not be applied to operator %s." (show exprType) (show opr)
    typedExpr = typeCheckExpr subExpr

-- | Type checks a binary expression.
typeCheckBinary :: (?context :: Context) => S.AtomicType -> S.BinaryOpr -> S.Expr -> S.Expr -> S.Expr
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
          error $ printf "Operator %s could not match operands: %s and %s." (show opr) (show leftType) (show rightType)
    typedLeft = typeCheckExpr leftExpr
    typedRight = typeCheckExpr rightExpr

-- | Type checks a conditional expression.
typeCheckConditional :: (?context :: Context) => S.AtomicType -> S.Expr -> S.Expr -> S.Expr -> S.Expr
typeCheckConditional _ condition trueExpr falseExpr = S.ConditionalExpr newType typedCondition typedTrue typedFalse
  where
    newType =
      case (getType typedTrue, getType typedFalse) of
        (trueType, falseType)
          | trueType == falseType -> trueType
          | otherwise ->
            error $
            printf
              "Conditional expression has unequal types in true [%s] and false [%s] side."
              (show trueType)
              (show falseType)
    typedCondition = typeCheckExprConforming condition S.BoolType
    typedTrue = typeCheckExpr trueExpr
    typedFalse = typeCheckExpr falseExpr

--                      --
-- Type check utilities --
--                      --
-- | Converts the given program parameter into a dummy normal parameter.
-- This is useful to check the global do block.
progParamToParam :: S.ProgParam -> S.Param
progParamToParam (S.ProgParam flowMode changeMode typedIdent) = S.Param flowMode (Just S.CopyMech) changeMode typedIdent

-- | Constructs a dummy ref parameter from the given global import syntax node.
-- TODO: Maybe consider adding typing to the global imports.
globalImportToParam :: (?context :: Context) => S.GlobalImport -> S.Param
globalImportToParam (S.GlobalImport flowMode changeMode name) =
  S.Param flowMode (Just S.RefMech) changeMode typedIdentifier
  where
    typedIdentifier = S.TypedIdentifier name globalType
    globalType =
      case find (\(S.StoreDeclaration _ (S.TypedIdentifier n _)) -> n == name) contextLocals of
        Nothing -> error $ printf "Can't find global %s." name
        Just (S.StoreDeclaration _ (S.TypedIdentifier _ t)) -> t

-- | Checks whether the supplied expression is a valid LExpr.
-- Currently, this is true only for NameExprs, naked variables.
isLExpr :: S.Expr -> Bool
isLExpr S.NameExpr {} = True
isLExpr _             = False

-- | Type checks the given expression and validates it against
-- the expected type. If successful, returns the newly typed expression.
typeCheckExprConforming :: (?context :: Context) => S.Expr -> S.AtomicType -> S.Expr
typeCheckExprConforming expr expectedType =
  if actualType /= expectedType
    then error $ printf "Expected type %s, but was: %s." (show expectedType) (show actualType)
    else typedExpr
  where
    typedExpr = typeCheckExpr expr
    actualType = getType typedExpr

-- | Type checks a function call or procedure application and returns
-- the type checked actual arguments.
-- The arguments are:
--   - The formal parameter declarations of the function/procedure.
--   - The global import declarations of the procedure (ignored/empty for functions).
--   - The actual argument expressions for the call.
--   - The global inits for the procedure call (empty for functions).
typeApplication :: (?context :: Context) => [S.Param] -> [S.GlobalImport] -> [S.Expr] -> [S.Ident] -> [S.Expr]
typeApplication formalParams globalImports args globInits =
  if globalsCheck globalImports globInits
    then typeArgs formalParams args
    else error "Error state: Globals check should error or be True."
  where
    globalsCheck _ [] = True
    globalsCheck imports (g:gs) =
      case find (\(S.GlobalImport _ _ name) -> name == g) imports of
        Nothing -> error $ printf "Tried to initialize global variable that is not used by procedure: %s." g
        Just _ -> globalsCheck imports gs
    typeArgs [] []         = []
    typeArgs _ []          = error "Not enough arguments in application."
    typeArgs [] _          = error "Too many arguments in application."
    typeArgs (p:ps) (a:as) = checkParam p a : typeArgs ps as
    checkParam param expr =
      if check
        then typedExpr
        else error "Error state: Param check should error or be True."
      where
        typedExpr = typeCheckExpr expr
        -- TODO: ChangeMode/Init checking.
        S.Param (Just flowMode) (Just mechMode) _ (S.TypedIdentifier paramName paramType) = param
        check
          | needsLExpr && not (isLExpr typedExpr) = error $ printf "LExpr required for parameter: %s." paramName
          | getType typedExpr /= paramType =
            error $
            printf
              "Param %s is of type %s, but expression of type %s provided."
              (show paramName)
              (show paramType)
              (show (getType typedExpr))
          | otherwise = True
        needsLExpr = (flowMode, mechMode) /= (S.InFlow, S.CopyMech)

-- | Extracts the return type of a function declaration.
typeOfFunction :: S.FunctionDeclaration -> S.AtomicType
typeOfFunction (S.FunctionDeclaration _ _ (S.StoreDeclaration _ (S.TypedIdentifier _ returnType)) _ _ _) = returnType

-- | Tries to lookup a function in the given context.
-- Fails with error if not found.
lookupFunction :: (?context :: Context) => S.Ident -> S.FunctionDeclaration
lookupFunction funcName =
  fromMaybe
    (error $ printf "Function not found: %s." funcName)
    (find (\(S.FunctionDeclaration n _ _ _ _ _) -> funcName == n) functions)
  where
    functions = contextFunctions

-- | Tries to lookup a procedure in the given context.
-- Fails with error if not found.
lookupProcedure :: (?context :: Context) => S.Ident -> S.ProcedureDeclaration
lookupProcedure procName =
  fromMaybe
    (error $ printf "Procedure not found: %s." procName)
    (find (\(S.ProcedureDeclaration n _ _ _ _) -> procName == n) procedures)
  where
    procedures = contextProcedures

-- | Tries to lookup the type of a variable in the given context.
-- Fails with error if not found.
lookupVariableType :: (?context :: Context) => S.Ident -> S.AtomicType
lookupVariableType varName =
  fromMaybe (error $ printf "Could not find variable named %s." varName) (typeAsParameter <|> typeAsLocal)
  where
    locals = contextLocals
    params = contextParams
    typeAsParameter =
      (\(S.Param _ _ _ (S.TypedIdentifier _ t)) -> t) <$>
      find (\(S.Param _ _ _ (S.TypedIdentifier n _)) -> varName == n) params
    typeAsLocal =
      (\(S.StoreDeclaration _ (S.TypedIdentifier _ t)) -> t) <$>
      find (\(S.StoreDeclaration _ (S.TypedIdentifier n _)) -> varName == n) locals
