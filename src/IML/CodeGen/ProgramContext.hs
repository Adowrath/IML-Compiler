module IML.CodeGen.ProgramContext
  ( Context(..)
  , checkContextIdentifiers
  , getAtomicTypeFromVarIdent
  , getAtomicTypeFromFuncIdent
  , searchIdentProcedures
  , isVarInContext
  , isFunInContext
  , isProcInContext
  , isMatchingFunction
  , isMatchingProcedure
  ) where

import           Control.Applicative   ((<|>))
import           Data.List
import           Data.Maybe            (fromMaybe)
import           IML.Parser.SyntaxTree

data Context =
  Context -- sammelt alle Variablen
    { progParams :: [ProgParam] -- programm parameters
    , functions  :: [FunctionDeclaration] -- Typenrelavant wegen return Value
    , procedures :: [ProcedureDeclaration] -- Möglich für Scope Check
    , params     :: [Param] -- function or procedure parameters
    , globals    :: [GlobalImport] -- global variables which should be reachable inside a function or a procedure
    , locals     :: [StoreDeclaration]
    }

searchIdentLocals :: Context -> Ident -> StoreDeclaration
searchIdentLocals c i =
  fromMaybe (error $ "Local identifier can not be found: " ++ i) (find (\x -> getIdent x == i) (locals c))

searchIdentGlobals :: Context -> Ident -> GlobalImport
searchIdentGlobals c i =
  fromMaybe (error $ "Global identifier can not be found: " ++ i) (find (\x -> getIdent x == i) (globals c))

searchIdentParams :: Context -> Ident -> Param
searchIdentParams c i =
  fromMaybe (error $ "Parameter identifier can not be found: " ++ i) (find (\x -> getIdent x == i) (params c))

searchIdentProgParams :: Context -> Ident -> ProgParam
searchIdentProgParams c i =
  fromMaybe
    (error $ "Program parameter identifier can not be found: " ++ i)
    (find (\x -> getIdent x == i) (progParams c))

searchIdentFunctions :: Context -> Ident -> FunctionDeclaration
searchIdentFunctions c i =
  fromMaybe (error $ "Function identifier can not be found: " ++ i) (find (\x -> getIdent x == i) (functions c))

searchIdentProcedures :: Context -> Ident -> ProcedureDeclaration
searchIdentProcedures c i =
  fromMaybe (error $ "Procedure identifier can not be found: " ++ i) (find (\sd -> getIdent sd == i) (procedures c))

getAtomicTypeFromVarIdent :: Context -> Ident -> AtomicType
getAtomicTypeFromVarIdent c i =
  fromMaybe
    (error $ "No atomic type can be found with: " ++ i)
    (localType <|> globalType <|> paramType <|> progParamType)
  where
    localType = getAtomicType c <$> findByIdent i (locals c)
    globalType = getAtomicType c <$> findByIdent i (globals c)
    paramType = getAtomicType c <$> findByIdent i (params c)
    progParamType = getAtomicType c <$> findByIdent i (progParams c)

getAtomicTypeFromFuncIdent :: Context -> Ident -> AtomicType
getAtomicTypeFromFuncIdent c i = fromMaybe (error $ "No atomic type can be found with: " ++ i) funcType
  where
    funcType = getAtomicType c <$> findByIdent i (functions c)

{-|
  Checks if there are any duplicates identifiers from and between functions and procedures.

  Takes a Context as record data.
  Returns True if they are disjoint sets, based on identifiers.
  Otherwise it will throw an error where the name clash happened.
-}
checkFunProPD :: Context -> Bool -- Check if Func-&Proc-Idents are parewise disjunct
checkFunProPD c
  | hasDuplicates fsIdents = error "Function name clash!"
  | hasDuplicates psIdents = error "Procedure name clash!"
  | hasDuplicates allIdents = error "At least one function name is the same as a procedure name!"
  | otherwise = True
  where
    fsIdents = getIdents $ functions c
    psIdents = getIdents $ procedures c
    allIdents = fsIdents ++ psIdents

{-|
  Checks if there are any duplicates identifiers from and between program-, function-/procedure-, global import-, local parameters.

  Takes a Context as record data.
  Returns True if they are disjoint sets, based on identifiers.
  Otherwise it will throw an error where the name clash happened.
-}
checkVarIdentPD :: Context -> Bool -- Check if Varaible-Idents are parewise disjunct
checkVarIdentPD c
  | hasDuplicates progParamIdents = error "Programm parameter name clash!"
  | hasDuplicates programIdents = error "Function/procedure parameter name clash!"
  | hasDuplicates globalIdents = error "Global import parameter name clash!"
  | hasDuplicates localIdents = error "Local parameter name clash!"
  | hasDuplicates allIdents =
    error "Name clash between programm- &| function-/procedure- &| global import- &| local parameter/s!, Have Fun ;-)"
  | otherwise = True
  where
    progParamIdents = getIdents $ progParams c
    programIdents = getIdents $ params c
    globalIdents = getIdents $ globals c
    localIdents = getIdents $ locals c
    allIdents = progParamIdents ++ programIdents ++ globalIdents ++ localIdents

{-|
  Checks if there are any name clashes in the context data.

  Takes a Context as record data.
  Returns True if all checks worked.
  Otherwise it will throw an error where the name clash happened.
-}
checkContextIdentifiers :: Context -> Bool
checkContextIdentifiers c = checkFunProPD c && checkVarIdentPD c

{-| 
  Check if an variable identifier can be found in Context.
-}
isVarInContext :: Context -> Ident -> Bool
isVarInContext c i = isInProgParams || isInParams || isInGlobals || isInLocals
  where isInProgParams  = foldl (\acc x -> acc || x) False (map (\x -> getIdent x == i) (progParams c))
        isInParams      = foldl (\acc x -> acc || x) False (map (\x -> getIdent x == i) (params c))
        isInGlobals     = foldl (\acc x -> acc || x) False (map (\x -> getIdent x == i) (globals c))
        isInLocals      = foldl (\acc x -> acc || x) False (map (\x -> getIdent x == i) (locals c))

{-| 
  Check if a Function identifier can be found in Context.
-}
isFunInContext :: Context -> Ident -> Bool
isFunInContext c i = foldl (\acc x -> acc || x) False (map (\x -> getIdent x == i) (functions c))

{-| 
  Check if a Function Header matches with Feunction Call.
  Based on suplied Type with Positions.
-}
isMatchingFunction :: Context -> Ident -> [Expr] -> Bool
isMatchingFunction c i es = allMatching
    where allMatching = foldl (\acc x -> acc && x) True (zipWith (\fp -> \e -> getAtomicType c fp == getAtomicType c e) fParams es)
          fParams = getParams (searchIdentFunctions c i)
          getParams (FunctionDeclaration _ params _ _ _ _) = params

{-| 
  Check if a Procedure identifier can be found in Context.
-}
isProcInContext :: Context -> Ident -> Bool
isProcInContext c i = foldl (\acc x -> acc || x) False (map (\x -> getIdent x == i) (procedures c))

{-| 
  Check if a Procedure Header matches with Procedure Call.
  Based on suplied Type with Positions.
-}
isMatchingProcedure :: Context -> Ident -> [Expr] -> Bool
isMatchingProcedure c i es = allMatching
    where allMatching = foldl (\acc x -> acc && x) True (zipWith (\fp -> \e -> getAtomicType c fp == getAtomicType c e) pParams es)
          pParams = getParams (searchIdentProcedures c i)
          getParams (ProcedureDeclaration _ params _ _ _) = params



class HasAtomicType a where
  getAtomicType :: Context -> a -> AtomicType

-- HELPER
instance HasAtomicType StoreDeclaration where
  getAtomicType c (StoreDeclaration _ typedIdentifier) = getAtomicType c typedIdentifier

instance HasAtomicType FunctionDeclaration where
  getAtomicType c (FunctionDeclaration _ _ storeDeclaration _ _ _) = getAtomicType c storeDeclaration

instance HasAtomicType GlobalImport where
  getAtomicType c (GlobalImport _ _ ident) = getAtomicTypeFromVarIdent c ident -- Search

instance HasAtomicType ProgParam where
  getAtomicType c (ProgParam _ _ typedIdentifier) = getAtomicType c typedIdentifier

instance HasAtomicType Param where
  getAtomicType c (Param _ _ _ typedIdentifier) = getAtomicType c typedIdentifier

instance HasAtomicType TypedIdentifier where
  getAtomicType _ (TypedIdentifier _ atomicType) = atomicType

instance HasAtomicType Expr where
  getAtomicType _ (LiteralExpr atomicType _)          = atomicType
  getAtomicType _ (FunctionCallExpr atomicType _ _)   = atomicType
  getAtomicType _ (NameExpr atomicType _ _)           = atomicType
  getAtomicType _ (UnaryExpr atomicType _ _)          = atomicType
  getAtomicType _ (BinaryExpr atomicType _ _ _)       = atomicType
  getAtomicType _ (ConditionalExpr atomicType _ _ _)  = atomicType

class HasIdent a where
  getIdent :: a -> Ident
  getIdents :: [a] -> [Ident]
  getIdents = map getIdent
  findByIdent :: Ident -> [a] -> Maybe a
  findByIdent i = find (\x -> getIdent x == i)

instance HasIdent Program where
  getIdent (Program ident _ _ _ _ _) = ident

instance HasIdent StoreDeclaration where
  getIdent (StoreDeclaration _ typedIdentifier) = getIdent typedIdentifier

instance HasIdent FunctionDeclaration where
  getIdent (FunctionDeclaration ident _ _ _ _ _) = ident

instance HasIdent ProcedureDeclaration where
  getIdent (ProcedureDeclaration ident _ _ _ _) = ident

instance HasIdent GlobalImport where
  getIdent (GlobalImport _ _ ident) = ident

instance HasIdent ProgParam where
  getIdent (ProgParam _ _ typedIdentifier) = getIdent typedIdentifier

instance HasIdent Param where
  getIdent (Param _ _ _ typedIdentifier) = getIdent typedIdentifier

instance HasIdent TypedIdentifier where
  getIdent (TypedIdentifier ident _) = ident

-- Herausfinden ob eine Liste Duplikate enthält
hasDuplicates :: (Eq a) => [a] -> Bool
hasDuplicates []     = False
hasDuplicates (x:xs) = elem x xs || hasDuplicates xs
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
