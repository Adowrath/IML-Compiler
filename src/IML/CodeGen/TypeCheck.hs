module IML.CodeGen.TypeCheck (
    module IML.CodeGen.TypeCheck,
    typeChecks
) where


-- import Data.List.Utils
import Data.List

import IML.CodeGen.CompileUtils
import qualified IML.CodeGen.ProgramContext as C

import qualified IML.Parser.SyntaxTree as S

import Data.Typeable


----------------------- Just For Manual Testing
import VirtualMachineIO as VM
import IML.Parser.Parser
import IML.Token.Tokenizer
import IML.CodeGen.DefaultsPhase as DP
exampleSimpleAdd :: IO S.Program
exampleSimpleAdd = do
  content <- readFile "./examples/SimpleAdd.iml"
  let parseResult = parse parseProgram $ tokenize content
  return $ fst $ head parseResult

filledSimpleAdd :: IO S.Program
filledSimpleAdd = DP.fillProgram <$> exampleSimpleAdd

typedSimpleAdd :: IO S.Program
typedSimpleAdd = typeChecks <$> filledSimpleAdd

-- compiledProgram :: IO VM.VMProgram
-- compiledProgram = compileProgram <$> typedSimpleAdd
-----------------------
{-|
    Checks the type and scope of a program.
    Returns the program with all atomic types set.
-}
typeChecks :: S.Program -> S.Program
typeChecks (S.Program ident programParams stores funcs procs commands) =
  if C.checkContextIdentifiers globalContext
    then S.Program ident programParams stores newFuncs newProcs newCommands
    else error "Context Error: Programm Global Identifier Fail"
  where newFuncs        = checkFDecl globalContext <$> funcs
        newProcs        = checkPDecl globalContext <$> procs
        newCommands     = checkCommands globalContext commands
        globalContext = C.Context {
            C.progParams  = programParams,
            C.functions   = funcs,
            C.procedures  = procs,
            C.params      = [],
            C.globals     = [],
            C.locals      = stores }

{-|
    Checks the type and scope of a function.
    Returns the function with all atomic types set.
-}
checkFDecl :: C.Context -> S.FunctionDeclaration -> S.FunctionDeclaration
checkFDecl c (S.FunctionDeclaration ident params storeDeclaration globalImports storeDeclarations commands) =
  if C.checkContextIdentifiers localContext
    then S.FunctionDeclaration ident newParams storeDeclaration globalImports storeDeclarations newCommands
    else error "Context Error: Programm Functions Identifier Fail"
  where newParams = map fillParamModes params -- adds all missing modes to the params
        newCommands = checkCommands localContext commands
        localContext = C.Context {
            C.progParams  = [],             -- Are not global visable
            C.functions   = C.functions c,  -- Stays the same during entire program
            C.procedures  = C.procedures c, -- Stays the same during entire program
            C.params      = newParams,
            C.globals     = globalImports ++ C.globals c,
            C.locals      = storeDeclaration : storeDeclarations } -- Returnvalue gets added to the Local Params

{-|
    Checks the type and scope of a procedure.
    Returns the procedure with all atomic types set.
-}
checkPDecl :: C.Context -> S.ProcedureDeclaration -> S.ProcedureDeclaration
checkPDecl c (S.ProcedureDeclaration ident params globalImports storeDeclarations commands) =
  if C.checkContextIdentifiers localContext
    then S.ProcedureDeclaration ident newParams globalImports storeDeclarations newCommands
    else error "Context Error: Programm Procedures Identifier Fail"
  where newParams = map fillParamModes params -- adds all missing modes to the params
        newCommands = checkCommands localContext commands
        localContext = C.Context {
            C.progParams  = [],             -- Are not global visable
            C.functions   = C.functions c,  -- Stays the same during entire program
            C.procedures  = C.procedures c, -- Stays the same during entire program
            C.params      = newParams,
            C.globals     = globalImports ++ C.globals c,
            C.locals      = storeDeclarations }

{-|
    Checks the type and scope of all commands.
    Returns all commands with all atomic types set.
-}
checkCommands :: C.Context -> [S.Command] -> [S.Command]
checkCommands context commandList = checkCommands' [] context commandList
    where checkCommands' acc _ [] = acc -- basecase
          checkCommands' acc c (co:cos) = checkCommands' (acc ++ [newCmd]) c cos
            where newCmd = case co of (S.SkipCommand)                         -> S.SkipCommand
                                      (S.AssignCommand exprl1 exprl2)         ->
                                        if checked && eqLen
                                          then S.AssignCommand newExprl1 newExprl2
                                          else error "Type Error: AssignCommand: unequal amount of expressions left and right"
                                        where newExprl1 = checkExpr c exprl1
                                              newExprl2 = checkExpr c exprl2
                                              checked   = and (zipWith (\e1 e2 -> (getExprAtomicType e1 == getExprAtomicType e2) && (isLExpr e1) || error "Assingment of two differnt types") newExprl1 newExprl2) -- check Type
                                              eqLen     = length newExprl1 == length newExprl2

                                      (S.IfCommand expr commandl1 commandl2)  ->
                                        if getExprAtomicType newExpr == S.BoolType -- check Type
                                          then S.IfCommand newExpr newCommandl1 newCommandl2
                                          else error "'If' supports only evaluation to boolean"
                                        where newExpr = checkExprSingle c expr -- set Type
                                              newCommandl1 = checkCommands c commandl1
                                              newCommandl2 = checkCommands c commandl2

                                      (S.WhileCommand expr commandl)          ->
                                        if getExprAtomicType newExpr == S.BoolType -- check Type
                                          then S.WhileCommand newExpr newCommandl
                                          else error "'While' supports only evaluation to boolean"
                                        where newExpr = checkExprSingle c expr -- set Type
                                              newCommandl = checkCommands c commandl
                                                          
                                      (S.CallCommand ident exprl1 idents)     -> 
                                        if C.isProcInContext c ident  -- check if all Types are correct
                                          then if C.isMatchingProcedure c ident newExprl1 -- TODO is exprl dijunct with idents ???
                                                 then S.CallCommand ident newExprl1 idents
                                                 else error ("Missmatch Error: Procedure decalartion with call: \"" ++ ident ++ "\"")
                                          else error ("Scope Error: Procedure \"" ++ ident ++ "\" is not in Scope.") -- NO Following !! it gets checked during the "checkPDecl"
                                        where newExprl1 = checkExpr c exprl1 
                                        
                                      (S.DebugInCommand expr)                 -> S.DebugInCommand newExpr
                                        where newExpr = checkExprSingle c expr -- set Type

                                      (S.DebugOutCommand expr)                -> S.DebugOutCommand newExpr
                                        where newExpr = checkExprSingle c expr -- set Type

{-|
    Checks the type and scope of all expressions.
    Returns all expressions with all atomic types set.
-}
checkExpr :: C.Context -> [S.Expr] -> [S.Expr] -- no Following where Function-Calls or Procedure-Calls
checkExpr context exprList = checkExpr' [] context exprList
    where checkExpr' acc _ [] = acc -- basecase
          checkExpr' acc c (e:es) = checkExpr' (acc ++ [newExpr]) c es
            where newExpr = checkExprSingle c e

{-|
    Checks the type and scope of an expression.
    Returns an expression with all atomic types set.
-}
checkExprSingle :: C.Context -> S.Expr -> S.Expr
checkExprSingle c e = case e of (S.LiteralExpr atomicType literal)                  -> S.LiteralExpr (getAtomicTypeOfLiteral literal) literal -- No Checks needed, just setting AtomicType

                                (S.FunctionCallExpr atomicType ident exprl)         -> 
                                  if C.isFunInContext c ident -- Scope Check of identifier
                                    then if C.isMatchingFunction c ident newExprL -- Check if all expected Types match with given ones (Pos, Amount, Eq)
                                           then S.FunctionCallExpr (C.getAtomicTypeFromFuncIdent c ident) ident newExprL -- NO FOLLOWING !! gets already checked by "checkFDecl"
                                           else error ("Missmatch Error: Function declaration with call: \"" ++ ident ++ "\"")
                                    else error ("Scope Error: Function \"" ++ ident ++ "\" is not in Scope.")
                                  where newExprL = checkExpr c exprl 

                                (S.NameExpr atomicType ident bool)                  -> 
                                  if C.isVarInContext c ident -- Check if Ident in Scope
                                    then S.NameExpr (C.getAtomicTypeFromVarIdent c ident) ident bool
                                    else error ("Scope Error: Variable \"" ++ show e ++ "\" is not in Scope.")

                                (S.UnaryExpr atomicType unaryOpr expr)              ->
                                  if unaryOpr == S.Not
                                    then if getExprAtomicType newExpr == S.BoolType
                                           then S.UnaryExpr S.BoolType unaryOpr newExpr
                                           else error "'Not' does not match with any other type than boolean"
                                    else if unaryOpr == S.UnaryPlus || unaryOpr == S.UnaryMinus
                                          then if getExprAtomicType newExpr /= S.BoolType
                                                 then S.UnaryExpr S.Int64Type unaryOpr newExpr
                                                 else error "'UnaryPlus' and 'UnaryMinus' do not match with any other type than boolean"
                                          else error ("Unidentifiable Error: Unary operator: " ++ show unaryOpr)
                                  where newExpr = checkExprSingle c expr

                                (S.BinaryExpr atomicType binaryOpr expr1 expr2)     -> binaryOprSolver binaryOpr nex1 nex2
                                  where nex1 = checkExprSingle c expr1
                                        nex2 = checkExprSingle c expr2

                                (S.ConditionalExpr atomicType expr1 expr2 expr3)    ->
                                  if isBoolType
                                    then if equal
                                           then S.ConditionalExpr ty nex1 nex2 nex3
                                           else error "Type Error: Conditional do not support different branching types."
                                    else error "Type Error: Conditional must have bool type as condition."
                                  where nex1 = checkExprSingle c expr1
                                        nex2 = checkExprSingle c expr2
                                        nex3 = checkExprSingle c expr3
                                        isBoolType = getExprAtomicType nex1 == S.BoolType
                                        ty = getExprAtomicType nex2
                                        equal = ty == getExprAtomicType nex3


-- HELPERS
{-|
    Adds the atomic type to a binary expression.
    Returns the filled expression.
-}
binaryOprSolver :: S.BinaryOpr -> S.Expr -> S.Expr -> S.Expr
binaryOprSolver op expr1 expr2 = S.BinaryExpr newType op expr1 expr2
  where
    newType =
      case (getExprAtomicType expr1, op, getExprAtomicType expr2) of
        (S.Int64Type, S.MultOpr, S.Int64Type)   -> S.Int64Type
        (S.Int64Type, S.DivEOpr, S.Int64Type)   -> S.Int64Type
        (S.Int64Type, S.ModEOpr, S.Int64Type)   -> S.Int64Type
        (S.Int64Type, S.PlusOpr, S.Int64Type)   -> S.Int64Type
        (S.Int64Type, S.MinusOpr, S.Int64Type)  -> S.Int64Type
        (S.Int64Type, S.LTOpr, S.Int64Type)     -> S.BoolType
        (S.Int64Type, S.GTOpr, S.Int64Type)     -> S.BoolType
        (S.Int64Type, S.LTEOpr, S.Int64Type)    -> S.BoolType
        (S.Int64Type, S.GTEOpr, S.Int64Type)    -> S.BoolType
        (lType, S.EqOpr, rType)
          | lType == rType                      -> S.BoolType
        (lType, S.NeqOpr, rType)
          | lType == rType                      -> S.BoolType
        (S.BoolType, S.CAndOpr, S.BoolType)     -> S.BoolType
        (S.BoolType, S.COrOpr, S.BoolType)      -> S.BoolType

{-|
    Takes an expression and returns the atomic type.
    Important the atomic type has already to be set for the expression.
-}
getExprAtomicType :: S.Expr -> S.AtomicType
getExprAtomicType (S.LiteralExpr atomicType _)            = if atomicType == S.Untyped then error "No AtomicType set" else atomicType
getExprAtomicType (S.FunctionCallExpr atomicType _ _)     = if atomicType == S.Untyped then error "No AtomicType set" else atomicType
getExprAtomicType (S.NameExpr atomicType _ _)             = if atomicType == S.Untyped then error "No AtomicType set" else atomicType
getExprAtomicType (S.UnaryExpr atomicType _ _)            = if atomicType == S.Untyped then error "No AtomicType set" else atomicType
getExprAtomicType (S.BinaryExpr atomicType _ _ _)         = if atomicType == S.Untyped then error "No AtomicType set" else atomicType
getExprAtomicType (S.ConditionalExpr atomicType _ _ _)    = if atomicType == S.Untyped then error "No AtomicType set" else atomicType

{-|
    Takes a literal and returns its atomic type.
-}
getAtomicTypeOfLiteral :: S.Literal -> S.AtomicType
getAtomicTypeOfLiteral l = case l of (S.BoolLiteral _)  -> S.BoolType
                                     (S.Int64Literal _) -> S.Int64Type

{-|
    Takes an expression and tells if it is an LValue.
-}
isLExpr :: S.Expr -> Bool
isLExpr (S.NameExpr _ _ _)  = True
isLExpr _                   = False

{-
getIdent :: e -> S.Ident
getIdent (S.Program ident _ _ _)                  = ident
getIdent (S.SDecl storeDeclaration)               = getIdent storeDeclaration
getIdent (S.FDecl functionDeclaration)            = getIdent functionDeclaration
getIdent (S.PDecl procedureDeclaration)           = getIdent procedureDeclaration
getIdent (S.StoreDeclaration _ typedIdentifier)   = getIdent typedIdentifier
getIdent (S.FunctionDeclaration ident _ _ _ _ _)  = ident
getIdent (S.ProcedureDeclaration ident _ _ _ _)   = ident
getIdent (S.GlobalImport _ _ ident)               = ident
getIdent (S.ProgParam _ _ typedIdentifier)        = getIdent typedIdentifier
getIdent (S.Param _ _ _ typedIdentifier)          = getIdent typedIdentifier
getIdent (S.TypedIdentifier ident _)              = ident
getIdent _                                        = error "given Element has no Identifier"
-}


{- TODO: Just for manual Testing
-- |
-- Fills in missing Modes into the given parameter
-- Defaults are "in", "copy", "const", with the following restrictions:
--
--   inout parameters cannot be const
--   in ref params cannot be var.
fillParamModes :: S.Param -> S.Param
fillParamModes (S.Param flowMode mechMode changeMode typedIdent) = S.Param (Just newFlow) (Just newMech) (Just newChange) typedIdent
  where (newFlow, newMech, newChange) = case (flowMode, mechMode, changeMode) of
          (Just S.InOutFlow , _               , Just S.ConstChange) -> error "inout <cm> const is not allowed"
          (Just S.InFlow    , Just S.RefMech  , Just S.VarChange)   -> error "in    ref  var   is not allowed"
          -- no flowmode with "ref var" defaults to "inout". TODO: Maybe this should be an error?
          (Nothing          , Just S.RefMech  , Just S.VarChange)   -> (S.InOutFlow , S.RefMech  , S.VarChange  )

          -- Defaults for inout are: copy var. const is not allowed. (handled above)
          (Just S.InOutFlow , _ , _) ->
            (S.InOutFlow,
              S.CopyMech `fromMaybe` mechMode,
              S.VarChange `fromMaybe` changeMode)

          -- Rest of the defaults:
          -- in is default, except for "ref var", which was handled above.
          (_, _, _) ->
            (S.InFlow `fromMaybe` flowMode,
              S.CopyMech `fromMaybe` mechMode,
              S.ConstChange `fromMaybe` changeMode)
-}
