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
{-
1. Alle Globlane Variablen sammeln
2. Alle Variablen der Main-Methode sammeln
3 Main methode überprüfen
3.1 Alle Funktions-Idents können als Variable mit dem Return Typ behandelt werden

Was Ist [Ident] in den CallCommands ???
Todo: Think about ProgramParams defaults and Restrictions

-}


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
typedSimpleAdd = typeChecks <$> exampleSimpleAdd

-- compiledProgram :: IO VM.VMProgram
-- compiledProgram = compileProgram <$> typedSimpleAdd
-----------------------

-- 3 Main-Groups
-- TODO Verbessrbar siehe DefaultPhases fillProgram
getFunctions :: ([S.FunctionDeclaration], [S.ProcedureDeclaration], [S.StoreDeclaration]) -> [S.FunctionDeclaration]
getFunctions (fs, _, _) = fs

getProcedures :: ([S.FunctionDeclaration], [S.ProcedureDeclaration], [S.StoreDeclaration]) -> [S.ProcedureDeclaration]
getProcedures (_, ps, _) = ps

getStoreDeclarations :: ([S.FunctionDeclaration], [S.ProcedureDeclaration], [S.StoreDeclaration]) -> [S.StoreDeclaration]
getStoreDeclarations (_, _, ss) = ss


typeChecks :: S.Program -> S.Program
typeChecks (S.Program ident programParams declarations commands) =
  if C.checkContextIdentifiers globalContext
    then S.Program ident programParams newDeclarations newCommands
    else error "Context Error: Programm Global Identifier Fail"
  where newDeclarations = checkDeclarations globalContext declarations
        newCommands     = checkCommands globalContext commands
        globalContext = C.Context {
                        C.progParams  = programParams,
                        C.functions   = getFunctions splitedDecs,
                        C.procedures  = getProcedures splitedDecs,
                        C.params      = [],
                        C.globals     = [],
                        C.locals      = getStoreDeclarations splitedDecs }
        splitedDecs = splitGlobalDeclarations declarations


checkDeclarations :: C.Context -> [S.Declaration] -> [S.Declaration]
checkDeclarations context declarationList = checkDeclarations' [] context declarationList
    where checkDeclarations' acc _ [] = acc -- basecase
          checkDeclarations' acc c (d:ds) = checkDeclarations' (acc ++ decel:[]) c ds
            where decel = case d of (S.FDecl fd) -> (S.FDecl (checkFDecl c fd))
                                    (S.PDecl pd) -> (S.PDecl (checkPDecl c pd))
                                     -- S.SDecl -> (S.SDecl (checkSDecl c d)) -- is not needed due to sdecl does not need any checks


checkFDecl :: C.Context -> S.FunctionDeclaration -> S.FunctionDeclaration
checkFDecl c (S.FunctionDeclaration ident params storeDeclaration globalImports storeDeclarations commands) =
  if C.checkContextIdentifiers localContext
    then S.FunctionDeclaration ident newParams storeDeclaration globalImports storeDeclarations newCommands
    else error "Context Error: Programm Functions Identifier Fail"
  where newParams = map fillParamModes params -- adds all missing modes to the params
        newCommands = checkCommands localContext commands
        localContext = C.Context {
          C.progParams  = [],           -- Are not global visable
          C.functions   = C.functions c,  -- Stays the same during entire program
          C.procedures  = C.procedures c, -- Stays the same during entire program
          C.params      = newParams,
          C.globals     = globalImports ++ (C.globals c),
          C.locals      = storeDeclaration : storeDeclarations } -- Returnvalue gets added to the Local Params

checkPDecl :: C.Context -> S.ProcedureDeclaration -> S.ProcedureDeclaration
checkPDecl c (S.ProcedureDeclaration ident params globalImports storeDeclarations commands) =
  if C.checkContextIdentifiers localContext
    then S.ProcedureDeclaration ident newParams globalImports storeDeclarations newCommands
    else error "Context Error: Programm Procedures Identifier Fail"
  where newParams = map fillParamModes params -- adds all missing modes to the params
        newCommands = checkCommands localContext commands
        localContext = C.Context {
          C.progParams  = [],           -- Are not global visable
          C.functions   = C.functions c,  -- Stays the same during entire program
          C.procedures  = C.procedures c, -- Stays the same during entire program
          C.params      = newParams,
          C.globals     = globalImports ++ (C.globals c),
          C.locals      = storeDeclarations }

-- Todo checkCommands :: C.Context -> [S.Command] -> [S.Command] -- no Following where Function-Calls or Procedure-Calls
checkCommands :: C.Context -> [S.Command] -> [S.Command] -- no Following where Function-Calls or Procedure-Calls
checkCommands context commandList = checkCommands' [] context commandList
    where checkCommands' acc _ [] = acc -- basecase
          checkCommands' acc c (co:cos) = checkCommands' (acc ++ [newCmd]) c cos
            where newCmd = case co of (S.SkipCommand)                         -> S.SkipCommand
                                      (S.AssignCommand exprl1 exprl2)         ->
                                        if checked
                                          then S.AssignCommand newExprl1 newExprl2
                                          else error "AssignCommand Type Error"
                                        where checked   = and (zipWith (\e1 -> \e2 -> if getExprAtomicType e1 == getExprAtomicType e1 then True else error "Assingment of two differnt types") newExprl1 newExprl2) -- check Type -- TODO: Hinzufügen auf Links darf es nur Variablen Namen haben "NamedExpr"
                                              newExprl1 = checkExpr c exprl1
                                              newExprl2 = checkExpr c exprl2

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
                                        if typeOf (C.searchIdentProcedures c ident) == typeOf S.ProcedureDeclaration -- check Type
                                          then S.CallCommand ident newExprl1 idents
                                          else error ("Procedure identifier not found: " ++ ident) -- NO Following !! it gets checked during the "checkPDecl"
                                        where newExprl1 = checkExpr c exprl1

                                      (S.DebugInCommand expr)                 -> S.DebugInCommand newExpr
                                        where newExpr = checkExprSingle c expr -- set Type

                                      (S.DebugOutCommand expr)                -> S.DebugOutCommand newExpr
                                        where newExpr = checkExprSingle c expr -- set Type


checkExpr :: C.Context -> [S.Expr] -> [S.Expr] -- no Following where Function-Calls or Procedure-Calls
checkExpr context exprList = checkExpr' [] context exprList
    where checkExpr' acc _ [] = acc -- basecase
          checkExpr' acc c (e:es) = checkExpr' (acc ++ [newExpr]) c es
            where newExpr = checkExprSingle c e

checkExprSingle :: C.Context -> S.Expr -> S.Expr
checkExprSingle c e = case e of (S.LiteralExpr atomicType literal)                  -> S.LiteralExpr (getAtomicTypeOfLiteral literal) literal -- No Checks needed, just setting AtomicType

                                (S.FunctionCallExpr atomicType ident exprl)         -> S.FunctionCallExpr (C.getAtomicTypeFromFuncIdent c ident) ident newExprL -- NO FOLLOWING !! gets already checked by "checkFDecl"
                                  where newExprL = checkExpr c exprl

                                (S.NameExpr atomicType ident bool)                  -> S.NameExpr (C.getAtomicTypeFromVarIdent c ident) ident bool

                                (S.UnaryExpr atomicType unaryOpr expr)              ->
                                  if unaryOpr == S.Not
                                    then if (getExprAtomicType expr) == S.BoolType then (S.UnaryExpr S.BoolType unaryOpr newExpr) else error "'Not' does not match with any other type than boolean"
                                    else if unaryOpr == S.UnaryPlus || unaryOpr == S.UnaryMinus
                                          then if (getExprAtomicType expr) /= S.BoolType then (S.UnaryExpr S.Int64Type unaryOpr newExpr) else error "'UnaryPlus' and 'UnaryMinus' do not match with any other type than boolean"
                                          else error ("No recognisable unary operator: " ++ (show unaryOpr))
                                  where newExpr = checkExprSingle c expr

                                (S.BinaryExpr atomicType binaryOpr expr1 expr2)     -> binaryOprSolver binaryOpr nex1 nex2
                                  where nex1 = checkExprSingle c expr1
                                        nex2 = checkExprSingle c expr2

                                (S.ConditionalExpr atomicType expr1 expr2 expr3)    ->
                                  if equal && isBoolType
                                    then S.ConditionalExpr ty nex1 nex2 nex3
                                    else error "Conditional do not support different types"
                                  where nex1 = checkExprSingle c expr1
                                        isBoolType = (getExprAtomicType nex1) == S.BoolType
                                        nex2 = checkExprSingle c expr2
                                        nex3 = checkExprSingle c expr3
                                        ty = getExprAtomicType nex2
                                        equal = ty == (getExprAtomicType nex3)


binaryOprSolver :: S.BinaryOpr -> S.Expr -> S.Expr -> S.Expr
binaryOprSolver op expr1 expr2 = S.BinaryExpr newType op expr1 expr2
  where
    newType =
      case (getExprAtomicType expr1, op, getExprAtomicType expr2) of
        (S.Int64Type, S.MultOpr, S.Int64Type) -> S.Int64Type
        (S.Int64Type, S.DivEOpr, S.Int64Type) -> S.Int64Type
        (S.Int64Type, S.ModEOpr, S.Int64Type) -> S.Int64Type
        (S.Int64Type, S.PlusOpr, S.Int64Type) -> S.Int64Type
        (S.Int64Type, S.MinusOpr, S.Int64Type) -> S.Int64Type
        (S.Int64Type, S.LTOpr, S.Int64Type) -> S.BoolType
        (S.Int64Type, S.GTOpr, S.Int64Type) -> S.BoolType
        (S.Int64Type, S.LTEOpr, S.Int64Type) -> S.BoolType
        (S.Int64Type, S.GTEOpr, S.Int64Type) -> S.BoolType
        (lType, S.EqOpr, rType)
          | lType == rType -> S.BoolType
        (lType, S.NeqOpr, rType)
          | lType == rType -> S.BoolType
        (S.BoolType, S.CAndOpr, S.BoolType) -> S.BoolType
        (S.BoolType, S.COrOpr, S.BoolType) -> S.BoolType

{-
isBoolTypeOpr :: (Eq a) => a -> Bool -- a als Spez Wert
isBoolTypeOpr S.COrOpr   = True
isBoolTypeOpr S.CAndOpr  = True
isBoolTypeOpr S.NeqOpr   = True
isBoolTypeOpr S.EqOpr    = True
isBoolTypeOpr S.Not      = True
isBoolTypeOpr _          = False

isIntTypeOper :: (Eq a) => a -> Bool
isIntTypeOper S.EqOpr   = True
isIntTypeOper S.NeqOpr  = True
isIntTypeOper o         = not (isBoolTypeOpr o)
isIntTypeOper _         = False -- most possibly not triggered
-}


-- AtomicType setting
setExprAtomicType :: C.Context -> S.Expr -> S.Expr
setExprAtomicType c e = case e of (S.LiteralExpr atomicType literal)                -> (S.LiteralExpr (getAtomicTypeOfLiteral literal) literal)
                                  (S.FunctionCallExpr atomicType ident exprl)       -> (S.FunctionCallExpr (C.getAtomicTypeFromFuncIdent c ident) ident exprl)
                                  (S.NameExpr atomicType ident bool)                -> (S.NameExpr (C.getAtomicTypeFromVarIdent c ident) ident bool)
                                  (S.UnaryExpr atomicType unaryOpr expr)            -> (S.UnaryExpr (getExprAtomicType (setExprAtomicType c expr)) unaryOpr expr)
                                  (S.BinaryExpr atomicType binaryOpr expr1 expr2)   -> (S.BinaryExpr (getExprAtomicType (setExprAtomicType c expr1)) binaryOpr expr1 expr2)
                                  (S.ConditionalExpr atomicType expr1 expr2 expr3)  -> (S.ConditionalExpr (getExprAtomicType (setExprAtomicType c expr2)) expr1 expr2 expr3)

getExprAtomicType :: S.Expr -> S.AtomicType -- TODO needs to grab the type if Untyped
getExprAtomicType (S.LiteralExpr atomicType _)            = if atomicType == S.Untyped then error "No AtomicType set" else atomicType
getExprAtomicType (S.FunctionCallExpr atomicType _ _)     = if atomicType == S.Untyped then error "No AtomicType set" else atomicType
getExprAtomicType (S.NameExpr atomicType _ _)             = if atomicType == S.Untyped then error "No AtomicType set" else atomicType
getExprAtomicType (S.UnaryExpr atomicType _ _)            = if atomicType == S.Untyped then error "No AtomicType set" else atomicType
getExprAtomicType (S.BinaryExpr atomicType _ _ _)         = if atomicType == S.Untyped then error "No AtomicType set" else atomicType
getExprAtomicType (S.ConditionalExpr atomicType _ _ _)    = if atomicType == S.Untyped then error "No AtomicType set" else atomicType

{- TODO Ungenutzt
getTypeVariableByIdent :: C.Context -> S.Ident -> S.AtomicType
getTypeVariableByIdent c i = do
    let l   = C.searchLocals c i
    let g   = C.searchGlobals c i
    let p   = C.searchParams c i
    let pp  = C.searchProgParams c i
    return (l || g || p || pp)
-}


-- HELPERS
getAtomicTypeOfLiteral :: S.Literal -> S.AtomicType
getAtomicTypeOfLiteral l = case l of (S.BoolLiteral _)  -> S.BoolType
                                     (S.Int64Literal _) -> S.Int64Type

compair2Types :: S.AtomicType -> S.AtomicType -> Bool
compair2Types a b = a == b

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





-- -######- Code below needs to be redone -######-
{-
-- Iteriert durch alle Commands und gibt am ende True wenn alle Typen stimmen
typeCheckCommands :: C.Context -> [S.Command] -> [S.Command] -- Tailrecursion
typeCheckCommands c l = typeCheckMain' True c l
    where typeCheckCommands' acc _  []                                                 = acc
          typeCheckCommands' acc co ((S.SkipCommand)                          :commands) = typeCheckCommands' acc co commands
          typeCheckCommands' acc co ((S.AssignCommand exprLi1 exprLi2)        :commands) = typeCheckCommands' (typeChecked && scopeChecked) co commands
            where typeChecked   = checkTrue (zipWith (\e1 -> \e2 -> getExprType e1 == getExprType e2) exprLi1 exprLi2)
                  scopeChecked  = checkTrue (map (\e -> checkScope co e) exprLi1) && checkTrue (map (\e -> checkScope co e) exprLi2)

          typeCheckCommands' acc co ((S.IfCommand expr commands1 commands2)   :commands) = typeCheckCommands' (typeChecked && scopeChecked) co commands
            where typeChecked   = (typeCheckExpr co expr) && (typeCheckCommands co commands1) && (typeCheckCommands co commands2)
                  scopeChecked  = False

          typeCheckCommands' acc co ((S.WhileCommand expr commands1)          :commands) = typeCheckCommands' (typeChecked && scopeChecked) co commands
            where typeChecked   = (typeCheckExpr co expr) && (typeCheckCommands co commands1)
                  scopeChecked  = False

          typeCheckCommands' acc co ((S.CallCommand ident exprLi [S.Ident])     :commands) = typeCheckCommands' (typeChecked && scopeChecked) co commands  -- Ist ein Aufruf für eine Prozedur
            where typeChecked   = typeCheckProcedureCallParams co exprLi
                  scopeChecked  = scopeCheck co ident

          typeCheckCommands' acc co ((S.DebugInCommand expr)                  :commands) = typeCheckCommands' (typeChecked && scopeChecked) co commands
            where typeChecked   = typeCheckExpr co expr
                  scopeChecked  = False

          typeCheckCommands' acc co ((S.DebugOutCommand expr)                 :commands) = typeCheckCommands' (typeChecked && scopeChecked) co commands
            where typeChecked   = typeCheckExpr co expr
                  scopeChecked  = False

          checkTrue (b:bs) = b && checkTrue bs

typeCheckExpr :: C.Context -> [S.Expr] -> [S.Expr] -- Check for Scope and for Equal Type, left and right
typeCheckExpr c l = typeCheckExpr' True c l
    where typeCheckExpr' acc _  []                                            = acc
          typeCheckExpr' acc co ((S.LiteralExpr _ S.Literal)              :exprs) = typeCheckExpr' acc co expers -- keine checks nötig bei einem einzelne Type und da kein Ident, auch kein Scope check nötig
          typeCheckExpr' acc co ((S.FunctionCallExpr _ ident exprli)    :exprs) = typeCheckExpr' (typeChecked && scopeChecked) co exprs -- ist ein Aufruf für eine Funktion
            where typeChecked   = typeCheckFunctionCallParams co exprli
                  scopeChecked  = scopeCheck co ident

          typeCheckExpr' acc co ((S.NameExpr _ ident _)                 :exprs) = typeCheckExpr' (typeChecked && scopeChecked) co exprs
            where typeChecked   = True -- Es gibt nichts zu Überprüfen, da es nur ein Typ hat
                  scopeChecked  = scopeCheck co ident

          typeCheckExpr' acc co ((S.UnaryExpr _ unaryOpr expr)          :exprs) = typeCheckExpr' (typeChecked && scopeChecked) co exprs
            where typeChecked   = if unaryOpr == Not && getExprType expr == BoolType || (unaryOpr == UnaryPlus || unaryOpr == UnaryMinus) && getExprType expr == Int64Type then True else False
                  scopeChecked  =

          typeCheckExpr' acc co ((S.BinaryExpr _ S.BinaryOpr S.Expr S.Expr)   :exprs) = typeCheckExpr' (typeChecked && scopeChecked) co exprs
            where typeChecked   =
                  scopeChecked  =

          typeCheckExpr' acc co ((S.ConditionalExpr _ S.Expr S.Expr S.Expr)   :exprs) = typeCheckExpr' (typeChecked && scopeChecked) co exprs -- exp2 und expr3 müssen gleich sein
            where typeChecked   =
                  scopeChecked  =


getExprType :: S.Expr -> S.AtomicType -- Problem With AtomicType and S.Literal Type
getExprType (S.LiteralExpr atomicType _)          = atomicType -- 1. typ überprüfen 2. typ als AtomicType setzen 3. AtomicType zurückgeben
getExprType (S.FunctionCallExpr atomicType _ _)   = atomicType
getExprType (S.NameExpr atomicType _ _)           = atomicType
getExprType (S.UnaryExpr atomicType _ _)          = atomicType
getExprType (S.BinaryExpr atomicType _ _ _)       = atomicType
getExprType (S.ConditionalExpr atomicType _ _ _)  = atomicType

setExprAtomicType :: S.Expr -> S.Expr -- im Programm den Atomic Type setzen zu allen Expressions
setExprAtomicType = unimplemented

evaluateExprType :: C.Context -> S.Expr -> S.AtomicType
evaluateExprType c (S.LiteralExpr S.AtomicType (l _))                   = literalToAtomicType l
evaluateExprType c (S.FunctionCallExpr S.AtomicType ident _)            = searchReturnTypeOfFucntion c ident -- Muss immer ein Return Value haben
evaluateExprType c (S.NameExpr atomicType S.Ident Bool)                 = atomicType -- ist bereits der Typ
evaluateExprType c (S.UnaryExpr S.AtomicType S.UnaryOpr expr)             = evaluateExprType c expr -- sollte normalerweise eine NameExpr sein oder eine LiteralExpr
evaluateExprType c (S.BinaryExpr S.AtomicType S.BinaryOpr expr1 expr2)    = if expr1found == expr2found then expr1found else error (expr1found ++ " is not the same Type as " ++ expr2found)  -- nimmt den Type von a bei a + 3
    where expr1found = evaluateExprType c expr1
          expr2found = evaluateExprType c expr2
evaluateExprType c (S.ConditionalExpr S.AtomicType expr1 expr2 expr3)   = unimplemented
    where expr2found = evaluateExprType c expr2
          expr3found = evaluateExprType c expr3





-- -########-HELPERS-########-

literalToAtomicType :: S.Literal -> S.AtomicType
literalToAtomicType (l _) = if l == S.BoolLiteral then BoolType else Int64Type

searchReturnTypeOfFucntion :: C.Context -> S.Ident -> S.AtomicType
searchReturnTypeOfFucntion c i = getReturnTypeOfFunction fun
    where fun = find (\(S.FunctionDeclaration ident _ _ _ _ _) -> ident == i ) (functions c)

getReturnTypeOfFunction :: S.FunctionDeclaration -> S.AtomicType
getReturnTypeOfFunction (S.FunctionDeclaration _ _ (S.StoreDeclaration _ (S.TypedIdentifier _ atomicType) _ _ _) = atomicType



-- ############################################



checkFunProc :: [] -> bool
checkFunProc ((dec ty):fs) = case dec of S.SDecl ->
                                            S.FDecl ->
                                            S.PDecl -> checkPDecl ty

checkPDecl :: S.ProcedureDeclaration -> Bool
checkPDecl (S.FunctionDeclaration _ params S.StoreDeclaration globalImports stoDecls commands) =



checkMain :: [] -> bool


checkProgramHead :: [] -> bool


typeCheckFunctionCallParams :: C.Context -> [S.Expr] -> Bool


typeCheckExpr :: C.Context -> S.Expr -> Bool

-- Vergleichen von 2 Typen miteinander
typeCheck2Expr :: C.Context -> S.Expr -> S.Expr -> Bool
typeCheck2Expr S.LiteralExpr   S.LiteralExpr =
typeCheck2Expr S.NameExpr      S.LiteralExpr =
typeCheck2Expr S.LiteralExpr   S.NameExpr    =

getTypeFromParam :: S.Param -> S.AtomicType
getTypeFromParam (S.Param _ _ _ (S.TypedIdentifier _ ty)) = ty

getIdentFromParam :: S.Param -> S.Ident
getIdentFromParam (S.Param _ _ _ (S.TypedIdentifier i _)) = i
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
