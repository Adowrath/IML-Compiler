import qualified IML.Parser.SyntaxTree as Syntax

{-
1. Alle Globlane Variablen sammeln
2. Alle Variablen der Main-Methode sammeln
3 Main methode überprüfen
3.1 Alle Funktions-Idents können als Variable mit dem Return Typ behandelt werden

Ansatz Problem Mit Rekursion !!! Mögliche Lösung, eine Maximale Function/Procedur Call Tiefe

Was Ist [Ident] in den CallCommands ???

Was macht die Stor Declaration
-}

data Context = Context { -- sammelt alle Variablen
    globals :: [GlobalImport] -- ??? Wie kommt man an diese Typen ???
    params :: [Param]
    locals :: [StoreDeclaration]
    functions :: [FunctionDeclaration] -- Typenrelavant wegen return Value
    procedures :: [ProcedureDeclaration] -- Möglich für Scope Check
}

typeChecks :: Program -> Bool
typeChecks (Program _ programParams declarations commands) = do
    let mainContext = Context { globals = [],
                                params = programParams,
                                locals = [] }
    let mainContext = addFunctionsToContext mainContext declarations -- Hier würderde die Möglichkeit bestehen auch Prozeduren mit Nahmen aufzunehmen, wobei dann der Typ ein Noting wäre
    return (typeCheckCommands mainContext commands)

typeCheckCommands :: Context -> [Command] -> Bool -- Tailrecursion
typeCheckCommands c l = typeCheckMain' True c l
    where typeCheckCommands' acc _  []                                                 = acc
          typeCheckCommands' acc co ((SkipCommand)                          :commands) = typeCheckCommands' acc co commands
          typeCheckCommands' acc co ((AssignCommand exprLi1 exprLi2)        :commands) = typeCheckCommands' (zipWith (\e1 -> \e2 -> typeCheck2Expr co e1 e2) exprLi1 exprLi2) co commands
          typeCheckCommands' acc co ((IfCommand expr commands1 commands2)   :commands) = typeCheckCommands' ((typeCheckExpr co expr) && (typeCheckCommands co commands1) && (typeCheckCommands co commands2)) co commands
          typeCheckCommands' acc co ((WhileCommand expr commands1)          :commands) = typeCheckCommands' ((typeCheckExpr co expr) && (typeCheckCommands co commands1)) co commands
          typeCheckCommands' acc co ((CallCommand Ident exprLi [Ident])     :commands) = typeCheckCommands' (typeCheckFunctionCallParams co exprLi) co commands -- Unklar was [Ident] ist ???
          typeCheckCommands' acc co ((DebugInCommand expr)                  :commands) = typeCheckCommands' (typeCheckExpr co expr) co commands
          typeCheckCommands' acc co ((DebugOutCommand expr)                 :commands) = typeCheckCommands' (typeCheckExpr co expr) co commands






typeChecks :: [] -> bool
typeChecks (progIn:funcProc:main) = do 
    checkMain main
    checkFunProc funcProc

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

getTypeIfIdentFromParam :: Param -> Ident -> Maybe AtomicType
getTypeIfIdentFromParam p i = if getIdentFromParam p == i then getTypeFromParam p else Nothing


-- Problem GlobalImports liefern keine Type oder TypedIdentifier