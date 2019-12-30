module ProgramContext (
    Context,
    checkContextIdentifiers,
    getAtomicTypeFromVarIdent,
    getAtomicTypeFromFuncIdent,

) where

import Data.List

data Context = Context { -- sammelt alle Variablen
    progParams :: [ProgParam],  -- programm parameters
    functions :: [FunctionDeclaration], -- Typenrelavant wegen return Value
    procedures :: [ProcedureDeclaration] -- Möglich für Scope Check
    params :: [Param],          -- function or procedure parameters
    globals :: [GlobalImport],  -- global variables which should be reachable inside a function or a procedure
    locals :: [StoreDeclaration]
    }

searchIdentLocals :: Context -> Ident -> StoreDeclaration
searchIdentLocals c i = do
    let local = find (\sd -> getIdent sd == i) locals c
    if local /= Nothing then return local
                        else error "Local identifier can not be found"

searchIdentGlobals :: Context -> Ident -> GlobalImport
searchIdentGlobals c i = do
    let global = find (\gi -> getIdent gi == i) globals c
    if global /= Nothing then return global
                         else error "Global identifier can not be found"

searchIdentParams :: Context -> Ident -> Param
searchIdentParams c i = do
    let param = find (\pa -> getIdent pa == i) params c
    if param /= Nothing then return param
                        else error "Parameter identifier can not be found"

searchIdentProgParams :: Context -> Ident -> ProgParam
searchIdentProgParams c i = do
    let programParam = find (\pp -> getIdent pp == i) progParams c
    if programParam /= Nothing then return programParam
                               else error "Program parameter identifier can not be found"

searchIdentFunctions :: Context -> Ident -> FunctionDeclaration
searchIdentFunctions c i = do
    let function = find (\f -> getIdent f == i) functions c
    if function /= Nothing then return function
                           else error "Function identifier can not be found"

searchIdentProcedures :: Context -> Ident -> ProcedureDeclaration
searchIdentProcedures c i = do
    let procedure = find (\pr -> getIdent pr == i) procedures c
    if procedure /= Nothing then return procedure
                            else error "Procedure identifier can not be found"


getAtomicTypeFromVarIdent :: Context -> Ident -> AtomicType
getAtomicTypeFromVarIdent c i = do
    let element = find (\e -> getIdent e == i)
    let l = element (locals c)
    if l /= Nothing then return (getAtomicType c l) else _
    let g = element (globals c)
    if g /= Nothing then return (getAtomicType c g) else _
    let p = element (params c)
    if p /= Nothing then return (getAtomicType c p) else _
    let pp = element (progParams c)
    if pp /= Nothing then return (getAtomicType c pp) else error ("No AtomicType can be found with: " ++ i ++ " in Variables Context")

getAtomicTypeFromFuncIdent :: Context -> Ident -> AtomicType
getAtomicTypeFromVarIdent c i = do
    let element = find (\e -> getIdent e == i)
    let f = element (functions c)
    if f /= Nothing then return (getAtomicType c f) else error ("No AtomicType can be found with: " ++ i ++ " in Function Context")

{-|
  Checks if there are any duplicates identifiers from and between functions and procedures.

  Takes a Context as record data.
  Returns True if they are disjoint sets, based on identifiers.
  Otherwise it will throw an error where the name clash happened.
-}
checkFunProPD :: Context -> Bool -- Check if Func-&Proc-Idents are parewise disjunct
checkFunProPD c = do
    let fsIdents = getIdents (functions c)
    case (hasDuplicates fsIdents) of True -> error "Function name clash!"
    let psIdents = getIdents (procedures c)
    case (hasDuplicates psIdents) of True -> error "Procedure name clash!"
    return $ if (hasDuplicates (fsIdents ++ psIdents)) then error "At least one function name is the same as a procedure name!" else True -- TODO OPTIONAL could be nicer if it would only check if the elements from one list apear in the second list

{-|
  Checks if there are any duplicates identifiers from and between program-, function-/procedure-, global import-, local parameters.

  Takes a Context as record data.
  Returns True if they are disjoint sets, based on identifiers.
  Otherwise it will throw an error where the name clash happened.
-}
checkVarIdentPD :: Context -> Bool -- Check if Varaible-Idents are parewise disjunct
checkVarIdentPD = do
    let ppIds = getIdents (progParams c)
    case (hasDuplicates ppIds) of True -> error "Programm parameter name clash!"
    let pIds = getIdents (params c)
    case (hasDuplicates pIds) of True -> error "Function/procedure parameter name clash!"
    let gIds = getIdents (globals c)
    case (hasDuplicates gIds) of True -> error "Global import parameter name clash!"
    let lIds = getIdents (locals c)
    case (hasDuplicates lIds) of True -> error "Local parameter name clash!"
    return $ if (hasDuplicates (pIds ++ gIds ++ lIds ++ ppIds)) then error "Name clash between programm- &| function-/procedure- &| global import- &| local parameter/s!, Have Fun ;-)" else True -- Todo Maybe change error Message XD

{-|
  Checks if there are any name clashes in the context data.

  Takes a Context as record data.
  Returns True if all checks worked.
  Otherwise it will throw an error where the name clash happened.
-}
checkContextIdentifiers :: Context -> Bool
checkContextIdentifiers c = do
    return $ checkFunProPD c && checkVarIdentPD c


-- HELPER
getAtomicType :: Context -> e -> AtomicType
getAtomicType c x = case x of (SDecl storeDeclaration)                            -> getAtomicType storeDeclaration
                              (StoreDeclaration _ typedIdentifier)                -> getAtomicType typedIdentifier
                              (FDecl functionDeclaration)                         -> getAtomicType functionDeclaration
                              (FunctionDeclaration _ _ storeDeclaration _ _ _)    -> getAtomicType storeDeclaration
                              (GlobalImport _ _ ident)                            -> getAtomicTypeFromVarIdent c ident -- Search
                              (ProgParam _ _ typedIdentifier)                     -> getAtomicType typedIdentifier
                              (Param _ _ _ typedIdentifier)                       -> getAtomicType typedIdentifier
                              (TypedIdentifier _ atomicType)                      -> atomicType



-- Alle Identifiers von einer Liste bekommen
getIdents :: [a] -> [Ident]
getIdents l = map (\f -> getIdent f ) l

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

-- Herausfinden ob eine Liste Duplikate enthält
hasDuplicates :: (Eq a) => [a] -> Bool
hasDuplicates []        = False
hasDuplicates (x:xs)    = any (\e -> e == x) xs || hasDuplicates xs
{-
-- Later Use
alreadyVisited :: a -> [a] -> Bool
alreadyVisited x [] = False
alreadyVisited x (v:visited) = ???

repeated :: [a] -> Bool
repeated xs = go xs []
--                   ^--  initial visited list is empty
    where
        go [] _ = False
        go (x:xs) visited =
            if alreadyVisited x visited
                then ???        -- If it's already visited, do what?
                else ???        -- Otherwise?
-}