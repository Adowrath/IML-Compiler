
import 

data FunctionMetadata = FunctionMetadata {
  inGlobals :: [TypedIdent],
  inParams :: [TypedIdent],
  returnParam :: TypedIdent
}

data ProcedureMetadata = ProcedureMetadata {
    inGlobals :: [TypedIdent],
    otherGlobals :: [TypedIdent],
    inParams :: [TypedIdent],
    otherParams :: [TypedIdent]
} 

genVMProgram :: Syntax.Program -> VMProgram -- Main Function


checkFunProc :: [] -> bool


checkMain :: [] -> bool


checkProgramHead :: [] -> bool


genFunc :: FunctionDeclaration -> VMProgram
genFunv FunctionDeclaration = do
    genMeta = do
        returnParam
        outcopyParam
        inoutcopyParam
        incopyParam
        globalParam
        refParam -- generates No local Varaible
    

genProc :: ProcedureDeclaration -> VMProgram


genFuncMeta :: FunctionDeclaration -> FunctionMetadata
genFuncMeta (FunctionDeclaration _ Params Return Globals _ _) = 
    FunctionMetadata{ 
        inGlobals = Globals, 
        inParams = Params, 
        returnParam = Return
    }

genProcMeta :: ProcedureDeclaration -> ProcedureMetadata
genProcMeta (ProcedureDeclaration _ Params Globals _ _) =
    ProcedureMetadata {
        inGlobals = fst inOhterGloblas,
        otherGlobals = snd inOhterGloblas,
        inParams = fst inOtherParams
        otherParams = snd inOtherParams
    } -- Separation between InFlow and In- or InOutFlow --> ([InFlows], [In-/InOutFlows])
    where inOtherParams  = partition (\(Param FlowMode _ _ _)      -> FlowMode == (Just InFlow)) Params
          inOhterGloblas = partition (\(GlobalImport FlowMode _ _) -> FlowMode == (Just InFlow)) Globals

genStor :: StoreDeclaration -> VMProgram


genAssign :: Command -> VMProgram


genExpr :: Expr -> VMProgram


genMain :: [] -> VMProgram



