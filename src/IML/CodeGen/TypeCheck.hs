import Data.List.Utils

import qualified IML.Parser.SyntaxTree as Syntax

{-
1. Alle Globlane Variablen sammeln
2. Alle Variablen der Main-Methode sammeln
3 Main methode überprüfen
3.1 Alle Funktions-Idents können als Variable mit dem Return Typ behandelt werden

Was Ist [Ident] in den CallCommands ???

-}

data Context = Context { -- sammelt alle Variablen
    progParams :: [ProgParam],  -- programm parameters
    functions :: [FunctionDeclaration], -- Typenrelavant wegen return Value
    procedures :: [ProcedureDeclaration] -- Möglich für Scope Check
    params :: [Param],          -- function or procedure parameters
    globals :: [GlobalImport],  -- global variables which should be reachable inside a function or a procedure
    locals :: [StoreDeclaration] }

getFunctions :: [Declaration] -> [FunctionDeclaration]
getFunctions ds = map (\(dt fd) -> fd) (filter (\(dt d) -> dt == FDecl) ds)

getProcedures :: [Declaration] -> [ProcedureDeclaration]
getProcedures ds = map (\(dt pd) -> pd) (filter (\(dt _) -> dt == PDecl) ds)

getStoreDeclarations :: [Declaration] -> [StoreDeclaration]
getStoreDeclarations ds = map (\(dt pd) -> pd) (filter (\(dt _) -> dt == SDecl) ds)

checkFunProPD :: Context -> Bool -- Check if Func-&Proc-Idents are parewise disjunct
checkFunProPD = unimplemented -- Todo

checkVarIdentPD :: Context -> Bool -- Check if Varaible-Idents are parewise disjunct
checkVarIdentPD = unimplemented -- Todo

typeChecks :: Program -> Program
typeChecks (Program ident programParams declarations commands) = do
    let globalContext = Context {
        progParams  = programParams,
        functions   = getFunctions declarations,
        procedures  = getProcedures declarations,
        params      = [],
        globals     = getStoreDeclarations declarations,
        locals      = [] }
    -- Todo Check Context for Paarwise Disjunkt
    let newDeclarations = checkDeclarations globalContext declarations
    let newCommands     = checkCommands globalContext commands -- Todo
    return (Program ident programParams newDeclarations newCommands)

checkDeclarations :: Context -> [Declaration] -> [Declaration]
checkDeclarations context declarationList = checkDeclarations' [] context declarationList
    where checkDeclarations' acc _ [] = acc -- basecase
          checkDeclarations' acc c ((dt sd)  :ds) = checkDeclarations' (acc ++ decel:[]) c ds
            where decel = case dt of FDecl -> (FDecl (checkFDecl c d))
                                     PDecl -> (PDecl (checkPDecl c d))
                                     -- SDecl -> (SDecl (checkSDecl c d)) -- is not needed due to sdecl does not need any checks


checkFDecl :: Context -> FunctionDeclaration -> FunctionDeclaration
checkFDecl c (FunctionDeclaration ident params storeDeclaration globalImports storeDeclarations commands) = do
    let localContext = Context { -- Todo Check All bevore Adding
        progParams  = [],           -- Are not global visable
        functions   = functions c,  -- Stays the same during entire program
        procedures  = procedures c, -- Stays the same during entire program
        params      = storeDeclaration : params, -- Returnvalue gets added to the Function Params
        globals     = globalImports,
        locals      = storeDeclarations }
    -- Todo Check Context for Paarwise Disjunkt
    let newCommands = checkCommands localContext commands
    return (FunctionDeclaration ident params storeDeclaration globalImports storeDeclarations newCommands)

checkPDecl :: Context -> ProcedureDeclaration -> ProcedureDeclaration
checkPDecl c (ProcedureDeclaration ident params globalImports storeDeclarations commands) = do
    let localContext = Context { -- Todo Check All bevore Adding
        progParams  = [],           -- Are not global visable
        functions   = functions c,  -- Stays the same during entire program
        procedures  = procedures c, -- Stays the same during entire program
        params      = params,
        globals     = globalImports,
        locals      = storeDeclarations }
    -- Todo Check Context for Paarwise Disjunkt
    let newCommands = checkCommands localContext commands
    return (ProcedureDeclaration ident params globalImports storeDeclarations newCommands)

-- Todo checkCommands :: Context -> [Command] -> [Command] -- no Following where Function-Calls or Procedure-Calls
checkCommands :: Context -> [Command] -> [Command] -- no Following where Function-Calls or Procedure-Calls
checkCommands context commandList = checkCommands' [] context commandList
    where checkCommands' acc _ [] = acc -- basecase
          checkCommands' acc c (co:cos) = checkCommands' (acc ++ newCmd) co cos
            where newCmd = case co of (SkipCommand)                         -> (SkipCommand)
                                      (AssignCommand exprl1 exprl2)         -> -- Follow
                                      (IfCommand expr commandl1 commandl2)  -> -- Follow
                                      (WhileCommand expr commandl)          -> -- Follow
                                      (CallCommand ident exprl1 exprl2)     ->
                                      (DebugInCommand expr)                 -> -- Follow
                                      (DebugOutCommand expr)                -> -- Follow

-- Todo checkExpr :: Context -> [Expr] -> [Expr] -- no Following where Function-Calls or Procedure-Calls
-- Todo Relplace Placholder in AtomicType with real AtomicType
checkExpr :: Context -> [Expr] -> [Expr] -- no Following where Function-Calls or Procedure-Calls
checkExpr context exprList = checkExpr' [] context exprList
    where checkExpr' acc _ [] = acc -- basecase
          checkExpr' acc c (e:es) = checkExpr' (acc ++ newExpr) e es
            where newExpr = case e of (LiteralExpr atomicType literal)             -> -- Follow
                                      (FunctionCallExpr atomicType ident exprl)    ->
                                      (NameExpr atomicType ident bool)             -> -- Follow
                                      (UnaryExpr atomicType unaryOpr expr)         -> -- Follow
                                      (BinaryExpr atomicType binaryOpr expr expr)  -> -- Follow
                                      (ConditionalExpr atomicType expr expr expr)  -> -- Follow



-- -######- Code below needs to be redone -######-

-- Iteriert durch alle Commands und gibt am ende True wenn alle Typen stimmen
typeCheckCommands :: Context -> [Command] -> [Command] -- Tailrecursion
typeCheckCommands c l = typeCheckMain' True c l
    where typeCheckCommands' acc _  []                                                 = acc
          typeCheckCommands' acc co ((SkipCommand)                          :commands) = typeCheckCommands' acc co commands
          typeCheckCommands' acc co ((AssignCommand exprLi1 exprLi2)        :commands) = typeCheckCommands' (typeChecked && scopeChecked) co commands
            where typeChecked   = checkTrue (zipWith (\e1 -> \e2 -> getExprType e1 == getExprType e2) exprLi1 exprLi2)
                  scopeChecked  = checkTrue (map (\e -> checkScope co e) exprLi1) && checkTrue (map (\e -> checkScope co e) exprLi2)

          typeCheckCommands' acc co ((IfCommand expr commands1 commands2)   :commands) = typeCheckCommands' (typeChecked && scopeChecked) co commands
            where typeChecked   = (typeCheckExpr co expr) && (typeCheckCommands co commands1) && (typeCheckCommands co commands2)
                  scopeChecked  = False

          typeCheckCommands' acc co ((WhileCommand expr commands1)          :commands) = typeCheckCommands' (typeChecked && scopeChecked) co commands
            where typeChecked   = (typeCheckExpr co expr) && (typeCheckCommands co commands1)
                  scopeChecked  = False

          typeCheckCommands' acc co ((CallCommand ident exprLi [Ident])     :commands) = typeCheckCommands' (typeChecked && scopeChecked) co commands  -- Ist ein Aufruf für eine Prozedur
            where typeChecked   = typeCheckProcedureCallParams co exprLi
                  scopeChecked  = scopeCheck co ident

          typeCheckCommands' acc co ((DebugInCommand expr)                  :commands) = typeCheckCommands' (typeChecked && scopeChecked) co commands
            where typeChecked   = typeCheckExpr co expr
                  scopeChecked  = False

          typeCheckCommands' acc co ((DebugOutCommand expr)                 :commands) = typeCheckCommands' (typeChecked && scopeChecked) co commands
            where typeChecked   = typeCheckExpr co expr
                  scopeChecked  = False

          checkTrue (b:bs) = b && checkTrue bs

typeCheckExpr :: Context -> [Expr] -> [Expr] -- Check for Scope and for Equal Type, left and right
typeCheckExpr c l = typeCheckExpr' True c l
    where typeCheckExpr' acc _  []                                            = acc
          typeCheckExpr' acc co ((LiteralExpr _ Literal)              :exprs) = typeCheckExpr' acc co expers -- keine checks nötig bei einem einzelne Type und da kein Ident, auch kein Scope check nötig
          typeCheckExpr' acc co ((FunctionCallExpr _ ident exprli)    :exprs) = typeCheckExpr' (typeChecked && scopeChecked) co exprs -- ist ein Aufruf für eine Funktion
            where typeChecked   = typeCheckFunctionCallParams co exprli
                  scopeChecked  = scopeCheck co ident

          typeCheckExpr' acc co ((NameExpr _ ident _)                 :exprs) = typeCheckExpr' (typeChecked && scopeChecked) co exprs
            where typeChecked   = True -- Es gibt nichts zu Überprüfen, da es nur ein Typ hat
                  scopeChecked  = scopeCheck co ident

          typeCheckExpr' acc co ((UnaryExpr _ unaryOpr expr)          :exprs) = typeCheckExpr' (typeChecked && scopeChecked) co exprs
            where typeChecked   = if unaryOpr == Not && getExprType expr == BoolType || (unaryOpr == UnaryPlus || unaryOpr == UnaryMinus) && getExprType expr == Int64Type then True else False
                  scopeChecked  =

          typeCheckExpr' acc co ((BinaryExpr _ BinaryOpr Expr Expr)   :exprs) = typeCheckExpr' (typeChecked && scopeChecked) co exprs
            where typeChecked   =
                  scopeChecked  =

          typeCheckExpr' acc co ((ConditionalExpr _ Expr Expr Expr)   :exprs) = typeCheckExpr' (typeChecked && scopeChecked) co exprs -- exp2 und expr3 müssen gleich sein
            where typeChecked   =
                  scopeChecked  =


getExprType :: Expr -> AtomicType -- Problem With AtomicType and Literal Type
getExprType (LiteralExpr atomicType _)          = atomicType -- 1. typ überprüfen 2. typ als AtomicType setzen 3. AtomicType zurückgeben
getExprType (FunctionCallExpr atomicType _ _)   = atomicType
getExprType (NameExpr atomicType _ _)           = atomicType
getExprType (UnaryExpr atomicType _ _)          = atomicType
getExprType (BinaryExpr atomicType _ _ _)       = atomicType
getExprType (ConditionalExpr atomicType _ _ _)  = atomicType

setExprAtomicType :: Expr -> Expr -- im Programm den Atomic Type setzen zu allen Expressions
setExprAtomicType = unimplemented

evaluateExprType :: Context -> Expr -> AtomicType
evaluateExprType c (LiteralExpr AtomicType (l _))                   = literalToAtomicType l
evaluateExprType c (FunctionCallExpr AtomicType ident _)            = searchReturnTypeOfFucntion c ident -- Muss immer ein Return Value haben
evaluateExprType c (NameExpr atomicType Ident Bool)                 = atomicType -- ist bereits der Typ
evaluateExprType c (UnaryExpr AtomicType UnaryOpr expr)             = evaluateExprType c expr -- sollte normalerweise eine NameExpr sein oder eine LiteralExpr
evaluateExprType c (BinaryExpr AtomicType BinaryOpr expr1 expr2)    = if expr1found == expr2found then expr1found else error (expr1found ++ " is not the same Type as " ++ expr2found)  -- nimmt den Type von a bei a + 3
    where expr1found = evaluateExprType c expr1
          expr2found = evaluateExprType c expr2
evaluateExprType c (ConditionalExpr AtomicType expr1 expr2 expr3)   = unimplemented
    where expr2found = evaluateExprType c expr2
          expr3found = evaluateExprType c expr3





-- -########-HELPERS-########-

literalToAtomicType :: Literal -> AtomicType
literalToAtomicType (l _) = if l == BoolLiteral then BoolType else Int64Type

searchReturnTypeOfFucntion :: Context -> Ident -> AtomicType
searchReturnTypeOfFucntion c i = getReturnTypeOfFunction fun
    where fun = find (\(FunctionDeclaration ident _ _ _ _ _) -> ident == i ) (functions c)

getReturnTypeOfFunction :: FunctionDeclaration -> AtomicType
getReturnTypeOfFunction (FunctionDeclaration _ _ (StoreDeclaration _ (TypedIdentifier _ atomicType) _ _ _) = atomicType



-- ############################################



checkFunProc :: [] -> bool
checkFunProc ((dec ty):fs) = case dec of SDecl ->  
                                            FDecl ->
                                            PDecl -> checkPDecl ty

checkPDecl :: ProcedureDeclaration -> Bool
checkPDecl (FunctionDeclaration _ params StoreDeclaration globalImports stoDecls commands) =



checkMain :: [] -> bool


checkProgramHead :: [] -> bool


typeCheckFunctionCallParams :: Context -> [Expr] -> Bool


typeCheckExpr :: Context -> Expr -> Bool

-- Vergleichen von 2 Typen miteinander
typeCheck2Expr :: Context -> Expr -> Expr -> Bool
typeCheck2Expr LiteralExpr   LiteralExpr =
typeCheck2Expr NameExpr      LiteralExpr =
typeCheck2Expr LiteralExpr   NameExpr    =

getTypeFromParam :: Param -> AtomicType
getTypeFromParam (Param _ _ _ (TypedIdentifier _ ty)) = ty

getIdentFromParam :: Param -> Ident
getIdentFromParam (Param _ _ _ (TypedIdentifier i _)) = i

