module IML.CodeGen.CodeGen where
{-
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
genVMProgram = do 
    scopeChecks
    typeChecks


genFunc :: FunctionDeclaration -> VMProgram
genFunv FunctionDeclaration = do
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
        inGlobals = Globals, -- sind nur InFlow
        inParams = Params, -- sind nur InFlow
        returnParam = Return
    }

genProcMeta :: ProcedureDeclaration -> ProcedureMetadata
genProcMeta (ProcedureDeclaration _ Params Globals _ _) =
    ProcedureMetadata {
        inGlobals = fst inOhterGloblas,
        otherGlobals = snd inOhterGloblas,
        inParams = fst inOtherParams,
        otherParams = snd inOtherParams
    } -- Separation between InFlow and In- or InOutFlow --> ([InFlows], [In-/InOutFlows])
    where inOtherParams  = partition (\(Param FlowMode _ _ _)      -> FlowMode == (Just InFlow)) Params
          inOhterGloblas = partition (\(GlobalImport FlowMode _ _) -> FlowMode == (Just InFlow)) Globals

genStor :: StoreDeclaration -> VMProgram


genAssign :: Command -> VMProgram


genExpr :: Expr -> VMProgram
genExpr | (LiteralExpr (Literal ty val)) = [(AllocBlock 1), (LoadIm vmty val)] 
            where vmty = case ty of (BoolLiteral Bool)      -> IntVmTy
                                    (Int64Literal Interger) -> Int64VmTy
        | (FunctionCallExpr Ident ExprList) -- ??
        | (NameExpr Ident Bool) = "Lade die Variable auf den Stack"
        | (UnaryExpr UnaryOpr Expr) = (genExpr Expr) ++ [operation]
                                      where operation = case UnaryOpr of Not          -> -- Unklar
                                                                         UnaryPlus    -> -- Unklar
                                                                         UnaryMinus   -> -- Unklar
        | (BinaryExpr BinaryOpr Expr1 Expr2) = (genExpr Expr1) ++ (genExpr Expr2) ++ [operation]
                                               where operation = case BinaryOpr of MultOpr    ->  (Mul Int64VmTy 0) -- 0 ist die Location ??
                                                                                   DivEOpr    ->  (DivEuclid Int64VmTy 0)
                                                                                   ModEOpr    ->  (ModEuclid Int64VmTy 0)
                                                                                   PlusOpr    ->  (Add Int64VmTy 0)
                                                                                   MinusOpr   ->  (Sub Int64VmTy 0)
                                                                                   LTOpr      ->  (Lt Int64VmTy)
                                                                                   GTOpr      ->  (Gt Int64VmTy)
                                                                                   LTEOpr     ->  (Le Int64VmTy)
                                                                                   GTEOpr     ->  (Ge Int64VmTy)
                                                                                   EqOpr      ->  (Eq Int64VmTy)
                                                                                   NeqOpr     ->  (Ne Int64VmTy)
                                                                                   CAndOpr    ->  (?? Int64VmTy) -- Was ist das
                                                                                   COrOpr     ->  (?? Int64VmTy) -- Was ist das
        | (ConditionalExpr Expr1 Expr2 Expr3) = (genExpr Expr1) "condJump if EXPR1 then EXPR2 else EXPR3"


genMain :: [] -> VMProgram



-}