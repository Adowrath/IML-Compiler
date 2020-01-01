module IML.CodeGen.TypeCheck (
    module IML.CodeGen.TypeCheck,
    typeChecks
) where

import Data.List.Utils
import Data.List

import IML.CodeGen.CompileUtils
import qualified IML.CodeGen.ProgramContext as C

import qualified IML.Parser.SyntaxTree as S

{-
1. Alle Globlane Variablen sammeln
2. Alle Variablen der Main-Methode sammeln
3 Main methode überprüfen
3.1 Alle Funktions-Idents können als Variable mit dem Return Typ behandelt werden

Was Ist [Ident] in den CallCommands ???
Todo: Think about ProgramParams defaults and Restrictions

-}

-- 3 Main-Groups
getFunctions :: ([S.FunctionDeclaration], [S.ProcedureDeclaration], [S.StoreDeclaration]) -> [FunctionDeclaration]
getFunctions (fs, _, _) = fs

getProcedures :: ([S.FunctionDeclaration], [S.ProcedureDeclaration], [S.StoreDeclaration]) -> [FunctionDeclaration]
getProcedures (_, ps, _) = ps

getStoreDeclarations :: ([S.FunctionDeclaration], [S.ProcedureDeclaration], [S.StoreDeclaration]) -> [FunctionDeclaration]
getStoreDeclarations (_, _, ss) = ss


typeChecks :: Program -> Program
typeChecks (Program ident programParams declarations commands) = do
    let splitedDecs = splitGlobalDeclarations declarations
    let globalContext = Context {
        progParams  = programParams,
        functions   = getFunctions splitedDecs,
        procedures  = getProcedures splitedDecs,
        params      = [],
        globals     = getStoreDeclarations splitedDecs,
        locals      = [] }
    C.checkContextIdentifiers globalContext -- could handle return True
    let newDeclarations = checkDeclarations globalContext declarations
    let newCommands     = checkCommands globalContext commands
    return (Program ident programParams newDeclarations newCommands)

checkDeclarations :: Context -> [Declaration] -> [Declaration]
checkDeclarations context declarationList = checkDeclarations' [] context declarationList
    where checkDeclarations' acc _ [] = acc -- basecase
          checkDeclarations' acc c (d:ds) = checkDeclarations' (acc ++ decel:[]) c ds
            where decel = case d of (FDecl fd) -> (FDecl (checkFDecl c fd))
                                    (PDecl pd) -> (PDecl (checkPDecl c pd))
                                     -- SDecl -> (SDecl (checkSDecl c d)) -- is not needed due to sdecl does not need any checks


checkFDecl :: Context -> FunctionDeclaration -> FunctionDeclaration
checkFDecl c (FunctionDeclaration ident params storeDeclaration globalImports storeDeclarations commands) = do
    let newParams = map fillParamModes params -- adds all missing modes to the params
    let localContext = Context {
        progParams  = [],           -- Are not global visable
        functions   = functions c,  -- Stays the same during entire program
        procedures  = procedures c, -- Stays the same during entire program
        params      = storeDeclaration : newParams, -- Returnvalue gets added to the Function Params
        globals     = globalImports ++ (globals c),
        locals      = storeDeclarations }
    C.checkContextIdentifiers localContext -- could handle return True
    let newCommands = checkCommands localContext commands
    return (FunctionDeclaration ident newParams storeDeclaration globalImports storeDeclarations newCommands)

checkPDecl :: Context -> ProcedureDeclaration -> ProcedureDeclaration
checkPDecl c (ProcedureDeclaration ident params globalImports storeDeclarations commands) = do
    let newParams = map fillParamModes params -- adds all missing modes to the params
    let localContext = Context {
        progParams  = [],           -- Are not global visable
        functions   = functions c,  -- Stays the same during entire program
        procedures  = procedures c, -- Stays the same during entire program
        params      = newParams,
        globals     = globalImports ++ (globals c),
        locals      = storeDeclarations }
    C.checkContextIdentifiers localContext -- could handle return True
    let newCommands = checkCommands localContext commands
    return (ProcedureDeclaration ident newParams globalImports storeDeclarations newCommands)

-- Todo checkCommands :: Context -> [Command] -> [Command] -- no Following where Function-Calls or Procedure-Calls
checkCommands :: Context -> [Command] -> [Command] -- no Following where Function-Calls or Procedure-Calls
checkCommands context commandList = checkCommands' [] context commandList
    where checkCommands' acc _ [] = acc -- basecase
          checkCommands' acc c (co:cos) = checkCommands' (acc ++ newCmd) co cos
            where newCmd = case co of (SkipCommand)                         -> (SkipCommand)
                                      (AssignCommand exprl1 exprl2)         -> assCmd
                                        where assCmd = do
                                                         newExprl1 <- checkExpr exprl1
                                                         newExprl2 <- checkExpr exprl2
                                                         and (zipWith (\e1 -> \e2 -> if getExprAtomicType e1 == getExprAtomicType e1 then True else error "Assingment of two differnt types") newExprl1 newExprl2)
                                                         return (AssignCommand newExprl1 newExprl2)
                                      (IfCommand expr commandl1 commandl2)  -> ifCmd
                                        where ifCmd = do
                                                        newExpr <- checkExprSingle expr -- set Type
                                                        equal <- getExprAtomicType newExpr == BoolType -- check Type
                                                        newCommandl1 <- checkCommands commandl1
                                                        newCommandl2 <- checkCommands commandl2
                                                        if equal then return (IfCommand expr newCommandl1 newCommandl2) else error "'If' supports only evaluation to boolean"
                                      (WhileCommand expr commandl)          -> whilCmd
                                        where whilCmd = do
                                                          newExpr <- checkExprSingle expr -- set Type
                                                          equal <- getExprAtomicType newExpr == BoolType -- check Type
                                                          newCommandl <- checkCommands commandl
                                                          if equal then return (WhileCommand newExpr newCommandl) else error "'While' supports only evaluation to boolean"
                                      (CallCommand ident exprl1 exprl2)     -> callCmd -- NO Following !! it gets checked during the "checkPDecl"
                                        where callCmd = do
                                                          newExprl1 <- checkExpr exprl1
                                                          newExprl2 <- checkExpr exprl2
                                                          if (C.searchIdentProcedures c ident) /= Nothing then return (CallCommand ident exprl1 exprl2) else error ("Procedure identifier not found: " ++ ident)
                                      (DebugInCommand expr)                 -> dbgInCmd
                                        where dbgInCmd = do
                                                           newExpr <- checkExprSingle expr -- set Type
                                                           return (DebugInCommand newExpr) -- TODO Does this expressen need to have a specific type ?
                                      (DebugOutCommand expr)                -> dbgOutCmd
                                        where dbgOutCmd = do
                                                            newExpr <- checkExprSingle expr -- set Type
                                                            return (DebugOutCommand newExpr)


checkExpr :: Context -> [Expr] -> [Expr] -- no Following where Function-Calls or Procedure-Calls
checkExpr context exprList = checkExpr' [] context exprList
    where checkExpr' acc _ [] = acc -- basecase
          checkExpr' acc c (e:es) = checkExpr' (acc ++ newExpr) e es
            where newExpr = checkExprSingle c e

checkExprSingle :: Context -> Expr -> Expr
checkExprSingle c e = case e of (LiteralExpr atomicType literal)                  -> (LiteralExpr (getAtomicTypeOfLiteral literal) literal) -- No Checks needed, just setting AtomicType

                                (FunctionCallExpr atomicType ident exprl)         -> (FunctionCallExpr (C.getAtomicTypeFromFuncIdent c ident) ident exprl) -- NO FOLLOWING !! gets already checked by "checkFDecl"

                                (NameExpr atomicType ident bool)                  -> (NameExpr (C.getAtomicTypeFromVarIdent c ident) ident bool)

                                (UnaryExpr atomicType unaryOpr expr)              -> unExp
                                    where unExp = do
                                                    newExpr <- checkExprSingle expr
                                                    return newUnaryExpr
                                                        where newUnaryExpr | isBoolTypeOpr unaryOpr = if (getExprAtomicType expr) == BoolType then (UnaryExpr BoolType unaryOpr expr) else error "'Not' does not match with any other type than boolean"
                                                                           | isIntTypeOper unaryOpr = if (getExprAtomicType expr) /= BoolType then (UnaryExpr Int64Type unaryOpr expr) else error "'UnaryPlus' and 'UnaryMinus' do not match with any other type than boolean"
                                                                           | otherwise              = error ("No recognisable unary operator: " ++ unaryOpr)

                                (BinaryExpr atomicType binaryOpr expr1 expr2)     -> binExp
                                  where binExp = do
                                                   nex1 <- checkExprSingle expr1
                                                   nex2 <- checkExprSingle expr2
                                                   ty <- getExprAtomicType nex1
                                                   equal <- ty == (getExprAtomicType nex2)
                                                   if not equal then error "Bianry operations need the same type on boath sides"
                                                                else if (isBoolTypeOpr binaryOpr && ty == BoolType) || (isIntTypeOper binaryOpr && ty == Int64Type) then return (BinaryExpr ty binaryOpr nex1 nex2)
                                                                                                                                                                    else error "Operator does not support given type"
                                (ConditionalExpr atomicType expr1 expr2 expr3)    -> conExp
                                  where conExp = do
                                                   nex1 <- checkExprSingle expr1
                                                   isBoolType <- (getExprAtomicType nex1) == BoolType
                                                   nex2 <- checkExprSingle expr2
                                                   nex3 <- checkExprSingle expr3
                                                   ty <- getExprAtomicType nex2
                                                   equal <- ty == (getExprAtomicType nex3)
                                                   if equal && isBoolType then return (ConditionalExpr ty nex1 nex2 nex3)
                                                                          else error "Conditional do not support different types"

isBoolTypeOpr :: (Eq a) => a -> Bool
isBoolTypeOpr o | o == COrOpr   = True
                | o == CAndOpr  = True
                | o == NeqOpr   = True
                | o == EqOpr    = True
                | o == Not      = True
                | otherwise     = False

isIntTypeOper :: (Eq a) => a -> Bool
isIntTypeOper o | o == EqOpr            = True
                | o == NeqOpr           = True
                | not (isBoolTypeOpr o) = True
                | otherwise             = False -- most possibly not triggered

-- AtomicType setting
setExprAtomicType :: Expr -> Expr
setExprAtomicType e = case e of (LiteralExpr atomicType literal)                -> (LiteralExpr (getAtomicTypeOfLiteral literal) literal)
                                (FunctionCallExpr atomicType ident exprl)       -> (FunctionCallExpr (C.getAtomicTypeFromFuncIdent ident) ident exprl)
                                (NameExpr atomicType ident bool)                -> (NameExpr (C.getAtomicTypeFromVarIdent ident) ident bool)
                                (UnaryExpr atomicType unaryOpr expr)            -> (UnaryExpr (getExprAtomicType (setExprAtomicType expr)) unaryOpr expr)
                                (BinaryExpr atomicType binaryOpr expr1 expr2)   -> (BinaryExpr (getExprAtomicType (setExprAtomicType expr1)) binaryOpr expr1 expr2)
                                (ConditionalExpr atomicType expr1 expr2 expr3)  -> (ConditionalExpr (getExprAtomicType (setExprAtomicType expr2)) expr1 expr2 expr3)

getExprAtomicType :: Expr -> AtomicType -- TODO needs to grab the type if Untyped
getExprAtomicType (LiteralExpr atomicType _)            = if atomicType == Untyped then error "No AtomicType set" else atomicType
getExprAtomicType (FunctionCallExpr atomicType _ _)     = if atomicType == Untyped then error "No AtomicType set" else atomicType
getExprAtomicType (NameExpr atomicType _ _)             = if atomicType == Untyped then error "No AtomicType set" else atomicType
getExprAtomicType (UnaryExpr atomicType _ _)            = if atomicType == Untyped then error "No AtomicType set" else atomicType
getExprAtomicType (BinaryExpr atomicType _ _ _)         = if atomicType == Untyped then error "No AtomicType set" else atomicType
getExprAtomicType (ConditionalExpr atomicType _ _ _)    = if atomicType == Untyped then error "No AtomicType set" else atomicType


getTypeVariableByIdent :: Context -> Ident -> AtomicType
getTypeVariableByIdent c i = do
    let l   = searchLocals c i
    let g   = searchGlobals c i
    let p   = searchParams c i
    let pp  = searchProgParams c i
    return (l || g || p || pp)



-- HELPERS
getAtomicTypeOfLiteral :: Literal -> AtomicType
getAtomicTypeOfLiteral l = case l of (BoolLiteral Bool)     -> BoolType
                                     (Int64Literal Integer) -> Int64Type

compair2Types :: AtomicType -> AtomicType -> Bool
compair2Types a b = a == b


getIdent :: e -> Ident
getIdent (Program ident _ _ _)                  = ident
getIdent (SDecl storeDeclaration)               = getIdent storeDeclaration
getIdent (FDecl functionDeclaration)            = getIdent functionDeclaration
getIdent (PDecl procedureDeclaration)           = getIdent procedureDeclaration
getIdent (StoreDeclaration _ typedIdentifier)   = getIdent typedIdentifier
getIdent (FunctionDeclaration ident _ _ _ _ _)  = ident
getIdent (ProcedureDeclaration ident _ _ _ _)   = ident
getIdent (GlobalImport _ _ ident)               = ident
getIdent (ProgParam _ _ typedIdentifier)        = getIdent typedIdentifier
getIdent (Param _ _ _ typedIdentifier)          = getIdent typedIdentifier
getIdent (TypedIdentifier ident _)              = ident
getIdent _                                      = error "given Element has no Identifier"






-- -######- Code below needs to be redone -######-
{-
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
-}
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
