module IML.CodeGen.CompileUtils
  ( getType
  , Context(..)
  , buildSubcontext
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
    { contextParams     :: [S.Param]
    , contextLocals     :: [S.StoreDeclaration]
    , contextFunctions  :: [S.FunctionDeclaration]
    , contextProcedures :: [S.ProcedureDeclaration]
    }
  deriving (Eq, Show)

-- | Constructs a subcontext from the given implicit context,
-- with new parameters and locals.
-- This is usable for functions' and procedures' own contexts.
buildSubcontext :: [S.Param] -> [S.StoreDeclaration] -> Context -> Context
buildSubcontext parameters locals context = context {contextParams = parameters, contextLocals = locals}
