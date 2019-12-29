module IML.CodeGen.CompileUtils where

import           Data.List             (foldl')
import qualified IML.Parser.SyntaxTree as S

-- | Split the given (global) declaration list into a triple of
-- unwrapped function, procedure and global variable declarations.
splitGlobalDeclarations :: [S.Declaration] -> ([S.FunctionDeclaration], [S.ProcedureDeclaration], [S.StoreDeclaration])
splitGlobalDeclarations = foldl' accumDecls ([], [], [])
  where
    accumDecls (fs, ps, ss) (S.FDecl fdecl) = (fdecl : fs, ps, ss)
    accumDecls (fs, ps, ss) (S.PDecl pdecl) = (fs, pdecl : ps, ss)
    accumDecls (fs, ps, ss) (S.SDecl sdecl) = (fs, ps, sdecl : ss)
