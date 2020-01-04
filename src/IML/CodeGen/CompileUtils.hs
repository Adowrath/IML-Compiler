{-# LANGUAGE ImplicitParams #-}

module IML.CodeGen.CompileUtils
  ( getType
  , Context
  , buildContext
  , buildSubcontext
  , contextParams
  , contextLocals
  , contextFunctions
  , contextProcedures
  ) where

import qualified IML.Parser.SyntaxTree as S

-- | Extracts the type from a given expression.
getType :: S.Expr -> S.AtomicType
getType (S.LiteralExpr exprType _)         = exprType
getType (S.FunctionCallExpr exprType _ _)  = exprType
getType (S.NameExpr exprType _ _)          = exprType
getType (S.UnaryExpr exprType _ _)         = exprType
getType (S.BinaryExpr exprType _ _ _)      = exprType
getType (S.ConditionalExpr exprType _ _ _) = exprType

-- | A context for the current location in the program.
-- Every procedure and function has a context that is
-- partially derived from the global context that applies to
-- the program's do block.
data Context =
  Context
    { _contextParams     :: [S.Param]
    , _contextLocals     :: [S.StoreDeclaration]
    , _contextFunctions  :: [S.FunctionDeclaration]
    , _contextProcedures :: [S.ProcedureDeclaration]
    }
  deriving (Eq, Show)

-- | Helper accessor with implicit context.
contextParams :: (?context :: Context) => [S.Param]
contextParams = _contextParams ?context

-- | Helper accessor with implicit context.
contextLocals :: (?context :: Context) => [S.StoreDeclaration]
contextLocals = _contextLocals ?context

-- | Helper accessor with implicit context.
contextFunctions :: (?context :: Context) => [S.FunctionDeclaration]
contextFunctions = _contextFunctions ?context

-- | Helper accessor with implicit context.
contextProcedures :: (?context :: Context) => [S.ProcedureDeclaration]
contextProcedures = _contextProcedures ?context

-- | Builds a context based off the provided parameters.
buildContext :: [S.Param] -> [S.StoreDeclaration] -> [S.FunctionDeclaration] -> [S.ProcedureDeclaration] -> Context
buildContext parameters locals functions procedures =
  Context
    { _contextParams = parameters
    , _contextLocals = locals
    , _contextFunctions = functions
    , _contextProcedures = procedures
    }

-- | Constructs a subcontext from the given implicit context,
-- with new parameters and locals.
-- This is usable for functions' and procedures' own contexts.
buildSubcontext :: (?context :: Context) => [S.Param] -> [S.StoreDeclaration] -> Context
buildSubcontext parameters locals = ?context {_contextParams = parameters, _contextLocals = locals}
