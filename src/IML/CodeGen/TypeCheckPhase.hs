module IML.CodeGen.TypeCheckPhase where

import           Control.Applicative      ((<|>))
import           Control.Monad.Reader
import           Data.List                (find)
import           Data.Maybe               (fromMaybe)
import           IML.CodeGen.CompileUtils (Context (..), buildSubcontext,
                                           getType)
import qualified IML.Parser.SyntaxTree    as S
import           Text.Printf

-- | A helper type for a function requiring a scope to
-- produce its result.
type Scoped a = Reader Context a

-- | Types the given program completely.
typeCheckProgram :: S.Program -> S.Program
typeCheckProgram (S.Program pName pParams stores funcs procs pCmds) =
  runReader
    (do typedFuncs <- mapM typeCheckFunction funcs
        typedProcs <- mapM typeCheckProcedure procs
        typedBlock <- typeCheckBlock pCmds
        return $ S.Program pName pParams stores typedFuncs typedProcs typedBlock)
    (Context (progParamToParam <$> pParams) stores funcs procs)

-- | Types the given function according to the supplied global context.
typeCheckFunction :: S.FunctionDeclaration -> Scoped S.FunctionDeclaration
typeCheckFunction (S.FunctionDeclaration name params retDecl globImps locals body) = do
  paramGlobals <- mapM globalImportToParam globImps
  newBody <- local (buildSubcontext (params ++ paramGlobals) (retDecl : locals)) $ typeCheckBlock body
  return $ S.FunctionDeclaration name params retDecl globImps locals newBody

-- | Types the given procedure according to the supplied global context.
typeCheckProcedure :: S.ProcedureDeclaration -> Scoped S.ProcedureDeclaration
typeCheckProcedure (S.ProcedureDeclaration name params globImps locals body) = do
  paramGlobals <- mapM globalImportToParam globImps
  newBody <- local (buildSubcontext (params ++ paramGlobals) locals) $ typeCheckBlock body
  return $ S.ProcedureDeclaration name params globImps locals newBody

--                  --
-- Command typings. --
--                  --
-- | Types the given block according to the supplied context.
typeCheckBlock :: [S.Command] -> Scoped [S.Command]
typeCheckBlock = mapM typeCheckCommand

-- | Types a given command in the supplied context.
typeCheckCommand :: S.Command -> Scoped S.Command
typeCheckCommand S.SkipCommand = pure S.SkipCommand
typeCheckCommand (S.AssignCommand leftExprs rightExprs) = typeCheckAssignment leftExprs rightExprs
typeCheckCommand (S.IfCommand condition thenBlock elseBlock) = typeCheckIf condition thenBlock elseBlock
typeCheckCommand (S.WhileCommand condition doBlock) = typeCheckWhile condition doBlock
typeCheckCommand (S.CallCommand procName procParams globInits) = typeCheckCall procName procParams globInits
typeCheckCommand (S.DebugInCommand expr) = do
  typedExpr <- typeCheckExpr expr
  if isLExpr typedExpr
    then return $ S.DebugInCommand typedExpr
    else fail $ printf "The expression to a debugin command must be an LExpr, but it is not. %s" (show typedExpr)
typeCheckCommand (S.DebugOutCommand expr) = S.DebugOutCommand <$> typeCheckExpr expr

-- | Type checks an assignment command.
typeCheckAssignment :: [S.Expr] -> [S.Expr] -> Scoped S.Command
typeCheckAssignment [] [] = fail "Assignment with empty sides."
typeCheckAssignment leftExprs rightExprs = do
  (leftTyped, rightTyped) <- checkAssignability leftExprs rightExprs
  return $ S.AssignCommand leftTyped rightTyped
  where
    checkAssignability [] [] = pure $ ([], [])
    checkAssignability [] _ = fail "Too many expressions on right hand side of assignment."
    checkAssignability _ [] = fail "Not enough expressions on right hand side of assignment."
    checkAssignability (l:ls) (r:rs) = do
      l' <- typeCheckExpr l
      r' <- typeCheckExpr r
      if isLExpr l' && (getType l' == getType r')
        then do
          (ls', rs') <- checkAssignability ls rs
          return $ (l' : ls', r' : rs')
        else fail $ printf "Cannot assign %s to %s." (show l') (show r')

-- | Type checks an if command.
typeCheckIf :: S.Expr -> [S.Command] -> [S.Command] -> Scoped S.Command
typeCheckIf condition thenBlock elseBlock =
  S.IfCommand <$> typeCheckExprConforming condition S.BoolType <*> typeCheckBlock thenBlock <*> typeCheckBlock elseBlock

-- | Type checks a while command.
typeCheckWhile :: S.Expr -> [S.Command] -> Scoped S.Command
typeCheckWhile condition doBlock =
  S.WhileCommand <$> typeCheckExprConforming condition S.BoolType <*> typeCheckBlock doBlock

-- | Type checks a call command.
typeCheckCall :: S.Ident -> [S.Expr] -> [S.Ident] -> Scoped S.Command
typeCheckCall procName paramExprs globInits = do
  (S.ProcedureDeclaration _ procParams globImports _ _) <- lookupProcedure procName
  typedParams <- typeApplication procParams globImports paramExprs globInits
  return $ S.CallCommand procName typedParams globInits

--                     --
-- Expression typings. --
--                     --
-- | Type checks an expression in the given typing context.
typeCheckExpr :: S.Expr -> Scoped S.Expr
typeCheckExpr (S.LiteralExpr tp lit) = typeCheckLiteral tp lit
typeCheckExpr (S.FunctionCallExpr tp name params) = typeCheckFunctionCall tp name params
typeCheckExpr (S.NameExpr tp name isInit) = typeCheckName tp name isInit
typeCheckExpr (S.UnaryExpr tp opr subExpr) = typeCheckUnary tp opr subExpr
typeCheckExpr (S.BinaryExpr tp opr leftExpr rightExpr) = typeCheckBinary tp opr leftExpr rightExpr
typeCheckExpr (S.ConditionalExpr tp condition trueExpr falseExpr) = typeCheckConditional tp condition trueExpr falseExpr

-- | Type checks a literal expression.
typeCheckLiteral :: S.AtomicType -> S.Literal -> Scoped S.Expr
typeCheckLiteral S.BoolType lit@(S.BoolLiteral _) = pure $ S.LiteralExpr S.BoolType lit
typeCheckLiteral S.Int64Type lit@(S.Int64Literal _) = pure $ S.LiteralExpr S.Int64Type lit
typeCheckLiteral tp lit = fail $ printf "Untyped or badly typed literal: %s with type %s." (show lit) (show tp)

-- | Type checks a function call.
typeCheckFunctionCall :: S.AtomicType -> S.Ident -> [S.Expr] -> Scoped S.Expr
typeCheckFunctionCall _ funcName paramExprs = do
  fun@(S.FunctionDeclaration _ funcParams _ _ _ _) <- lookupFunction funcName
  typedParams <- typeApplication funcParams [] paramExprs []
  return $ S.FunctionCallExpr (typeOfFunction fun) funcName typedParams

-- | Type checks a variable name in the given context.
typeCheckName :: S.AtomicType -> S.Ident -> Bool -> Scoped S.Expr
typeCheckName _ name isInit = S.NameExpr <$> lookupVariableType name <*> pure name <*> pure isInit

-- | Type checks a unary expression.
typeCheckUnary :: S.AtomicType -> S.UnaryOpr -> S.Expr -> Scoped S.Expr
typeCheckUnary _ opr expr = do
  typedExpr <- typeCheckExpr expr
  newType <-
    case (opr, getType typedExpr) of
      (S.Not, S.BoolType) -> pure S.BoolType
      (S.UnaryMinus, S.Int64Type) -> pure S.Int64Type
      (S.UnaryPlus, S.Int64Type) -> pure S.Int64Type
      (_, exprType) ->
        fail $ printf "Expression of type %s could not be applied to operator %s." (show exprType) (show opr)
  return $ S.UnaryExpr newType opr typedExpr

-- | Type checks a binary expression.
typeCheckBinary :: S.AtomicType -> S.BinaryOpr -> S.Expr -> S.Expr -> Scoped S.Expr
typeCheckBinary _ opr leftExpr rightExpr = do
  typedLeft <- typeCheckExpr leftExpr
  typedRight <- typeCheckExpr rightExpr
  newType <-
    case (getType typedLeft, opr, getType typedRight) of
      (S.Int64Type, S.MinusOpr, S.Int64Type) -> pure S.Int64Type
      (S.Int64Type, S.MultOpr, S.Int64Type) -> pure S.Int64Type
      (S.Int64Type, S.DivEOpr, S.Int64Type) -> pure S.Int64Type
      (S.Int64Type, S.DivFOpr, S.Int64Type) -> pure S.Int64Type
      (S.Int64Type, S.DivTOpr, S.Int64Type) -> pure S.Int64Type
      (S.Int64Type, S.ModEOpr, S.Int64Type) -> pure S.Int64Type
      (S.Int64Type, S.ModFOpr, S.Int64Type) -> pure S.Int64Type
      (S.Int64Type, S.ModTOpr, S.Int64Type) -> pure S.Int64Type
      (S.Int64Type, S.PlusOpr, S.Int64Type) -> pure S.Int64Type
      (S.Int64Type, S.LTEOpr, S.Int64Type) -> pure S.BoolType
      (S.Int64Type, S.GTEOpr, S.Int64Type) -> pure S.BoolType
      (S.Int64Type, S.LTOpr, S.Int64Type) -> pure S.BoolType
      (S.Int64Type, S.GTOpr, S.Int64Type) -> pure S.BoolType
      (lt, S.EqOpr, rt)
        | lt == rt -> pure S.BoolType
      (lt, S.NeqOpr, rt)
        | lt == rt -> pure S.BoolType
      (S.BoolType, S.CAndOpr, S.BoolType) -> pure S.BoolType
      (S.BoolType, S.COrOpr, S.BoolType) -> pure S.BoolType
      (leftType, _, rightType) ->
        fail $ printf "Operator %s could not match operands: %s and %s." (show opr) (show leftType) (show rightType)
  return $ S.BinaryExpr newType opr typedLeft typedRight

-- | Type checks a conditional expression.
typeCheckConditional :: S.AtomicType -> S.Expr -> S.Expr -> S.Expr -> Scoped S.Expr
typeCheckConditional _ condition trueExpr falseExpr = do
  typedCondition <- typeCheckExprConforming condition S.BoolType
  typedTrue <- typeCheckExpr trueExpr
  typedFalse <- typeCheckExpr falseExpr
  newType <-
    case (getType typedTrue, getType typedFalse) of
      (trueType, falseType)
        | trueType == falseType -> pure trueType
        | otherwise ->
          fail $
          printf
            "Conditional expression has unequal types in true [%s] and false [%s] side."
            (show trueType)
            (show falseType)
  return $ S.ConditionalExpr newType typedCondition typedTrue typedFalse

--                      --
-- Type check utilities --
--                      --
-- | Converts the given program parameter into a dummy normal parameter.
-- This is useful to check the global do block.
progParamToParam :: S.ProgParam -> S.Param
progParamToParam (S.ProgParam flowMode changeMode typedIdent) = S.Param flowMode (Just S.CopyMech) changeMode typedIdent

-- | Constructs a dummy ref parameter from the given global import syntax node.
-- TODO: Maybe consider adding typing to the global imports.
globalImportToParam :: S.GlobalImport -> Scoped S.Param
globalImportToParam (S.GlobalImport flowMode changeMode name) = do
  globals <- asks contextLocals
  globalType <-
    case find (\(S.StoreDeclaration _ (S.TypedIdentifier n _)) -> n == name) globals of
      Nothing -> fail $ printf "Can't find global %s." name
      Just (S.StoreDeclaration _ (S.TypedIdentifier _ t)) -> pure t
  return $ S.Param flowMode (Just S.RefMech) changeMode $ S.TypedIdentifier name globalType

-- | Checks whether the supplied expression is a valid LExpr.
-- Currently, this is true only for NameExprs, naked variables.
isLExpr :: S.Expr -> Bool
isLExpr S.NameExpr {} = True
isLExpr _             = False

-- | Type checks the given expression and validates it against
-- the expected type. If successful, returns the newly typed expression.
typeCheckExprConforming :: S.Expr -> S.AtomicType -> Scoped S.Expr
typeCheckExprConforming expr expectedType = do
  typedExpr <- typeCheckExpr expr
  let actualType = getType typedExpr
  if actualType /= expectedType
    then fail $ printf "Expected type %s, but was: %s." (show expectedType) (show actualType)
    else pure typedExpr

-- | Type checks a function call or procedure application and returns
-- the type checked actual arguments.
-- The arguments are:
--   - The formal parameter declarations of the function/procedure.
--   - The global import declarations of the procedure (ignored/empty for functions).
--   - The actual argument expressions for the call.
--   - The global inits for the procedure call (empty for functions).
typeApplication :: [S.Param] -> [S.GlobalImport] -> [S.Expr] -> [S.Ident] -> Scoped [S.Expr]
typeApplication formalParams globalImports args globInits =
  if not $ globalsCheck globalImports globInits
    then fail "Tried to initialize global variable that is not used by procedure." -- TODO: This might move to a future flow phase.
    else typeArgs formalParams args
  where
    globalsCheck imports inits = all (\i -> any (\(S.GlobalImport _ _ name) -> name == i) imports) inits
    typeArgs [] []         = pure []
    typeArgs _ []          = fail "Not enough arguments in application."
    typeArgs [] _          = fail "Too many arguments in application."
    typeArgs (p:ps) (a:as) = (:) <$> checkParam p a <*> typeArgs ps as
    checkParam (S.Param (Just flowMode) (Just mechMode) _ (S.TypedIdentifier paramName paramType)) expr = do
      typedExpr <- typeCheckExpr expr
      let needsLExpr = (flowMode, mechMode) /= (S.InFlow, S.CopyMech)
      if needsLExpr && not (isLExpr typedExpr)
        then fail $ printf "lExpr required for parameter: %s." paramName
        else if getType typedExpr /= paramType
               then fail $
                    printf
                      "Param %s is of type %s, but expression of type %s provided."
                      (show paramName)
                      (show paramType)
                      (show (getType typedExpr))
               else pure typedExpr

-- | Extracts the return type of a function declaration.
typeOfFunction :: S.FunctionDeclaration -> S.AtomicType
typeOfFunction (S.FunctionDeclaration _ _ (S.StoreDeclaration _ (S.TypedIdentifier _ returnType)) _ _ _) = returnType

-- | Tries to lookup a function in the given context.
-- Fails with error if not found.
lookupFunction :: S.Ident -> Scoped S.FunctionDeclaration
lookupFunction funcName = do
  functions <- asks contextFunctions
  case find (\(S.FunctionDeclaration n _ _ _ _ _) -> funcName == n) functions of
    Nothing   -> fail $ printf "Function not found: %s." funcName -- TODO: When Either and not Identity, we can change.
    Just func -> return func

-- | Tries to lookup a procedure in the given context.
-- Fails with error if not found.
lookupProcedure :: S.Ident -> Scoped S.ProcedureDeclaration
lookupProcedure procName = do
  procedures <- asks contextProcedures
  case find (\(S.ProcedureDeclaration n _ _ _ _) -> procName == n) procedures of
    Nothing   -> fail $ printf "Procedure not found: %s." procName -- TODO: When Either and not Identity, we can change.
    Just proc -> return proc

-- | Tries to lookup the type of a variable in the given context.
-- Fails with error if not found.
lookupVariableType :: S.Ident -> Scoped S.AtomicType
lookupVariableType varName = do
  locals <- asks contextLocals
  params <- asks contextParams
  let typeAsParameter =
        (\(S.Param _ _ _ (S.TypedIdentifier _ t)) -> t) <$>
        find (\(S.Param _ _ _ (S.TypedIdentifier n _)) -> varName == n) params
      typeAsLocal =
        (\(S.StoreDeclaration _ (S.TypedIdentifier _ t)) -> t) <$>
        find (\(S.StoreDeclaration _ (S.TypedIdentifier n _)) -> varName == n) locals
  return $ fromMaybe (error $ printf "Could not find variable named %s." varName) (typeAsParameter <|> typeAsLocal)
