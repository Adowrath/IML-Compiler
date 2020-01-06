module IML.CodeGen.DefaultsPhase where

import           Control.Applicative   ((<|>))
import           Data.Maybe            (fromMaybe)
import qualified IML.Parser.SyntaxTree as S

-- |
-- Fills in missing modes into the whole program.
-- See the other functions for a more thorough description.
fillProgram :: S.Program -> S.Program
fillProgram (S.Program name programParams stores functions procedures body) =
  S.Program name filledProgramParams filledStores filledFuncs filledProcs body
  where
    filledFuncs = fillFunction <$> functions
    filledProcs = fillProcedure <$> procedures
    filledStores = fillStoreDeclarationModes <$> stores
    filledProgramParams = fillProgParamModes <$> programParams

-- |
-- Fills in missing modes into the given global import declaration.
-- Defaults are "in" and "const", with the following restriction:
--
--   inout global imports cannot be const.
-- This is equal in functionality to fillProgParamModes.
fillGlobalImportModes :: S.GlobalImport -> S.GlobalImport
fillGlobalImportModes (S.GlobalImport flowMode changeMode name) = S.GlobalImport newFlow newChange name
  where
    (newFlow, newChange) =
      case (flowMode, changeMode) of
        (Just S.InOutFlow, Just S.ConstChange) -> error "inout const is not allowed as a global import"
        (Just S.InOutFlow, _) -- inout must be var.
         -> (flowMode, changeMode <|> Just S.VarChange)
        (_, _) -- Rest is "in const" by default
         -> (flowMode <|> Just S.InFlow, changeMode <|> Just S.ConstChange)

-- |
-- Fills in missing Modes into the given program parameter.
-- Defaults are "in" and "const", with the following restrictions:
--
--   inout parameters cannot be const
-- This is similar to fillParamModes, but program parameters are effectively copy.
fillProgParamModes :: S.ProgParam -> S.ProgParam
fillProgParamModes (S.ProgParam flowMode changeMode typedIdent) = S.ProgParam newFlow newChange typedIdent
  where
    (newFlow, newChange) =
      case (flowMode, changeMode) of
        (Just S.InOutFlow, Just S.ConstChange) -> error "inout const is not allowed as a program parameter"
        (Just S.InOutFlow, _) -- ^ inout must be var.
         -> (flowMode, changeMode <|> Just S.VarChange)
        (_, _) -- ^ Rest is "in const" by default
         -> (flowMode <|> Just S.InFlow, changeMode <|> Just S.ConstChange)

-- |
-- Fills in missing Modes into the given parameter
-- Defaults are "in", "copy", "const", with the following restrictions:
--
--   inout parameters cannot be const.
--   in ref params cannot be var.
fillParamModes :: S.Param -> S.Param
fillParamModes (S.Param flowMode mechMode changeMode typedIdent) =
  S.Param (Just newFlow) (Just newMech) (Just newChange) typedIdent
  where
    (newFlow, newMech, newChange) =
      case (flowMode, mechMode, changeMode) of
        (Just S.InOutFlow, _, Just S.ConstChange) -> error "inout <cm> const is not allowed"
        (Just S.InFlow, Just S.RefMech, Just S.VarChange) -> error "in    ref  var   is not allowed"
          -- no flowmode with "ref var" defaults to "inout". TODO: Maybe this should be an error?
        (Nothing, Just S.RefMech, Just S.VarChange) -> (S.InOutFlow, S.RefMech, S.VarChange)
          -- Defaults for inout are: copy var. const is not allowed. (handled above)
        (Just S.InOutFlow, _, _) -> (S.InOutFlow, S.CopyMech `fromMaybe` mechMode, S.VarChange `fromMaybe` changeMode)
          -- Rest of the defaults:
          -- in is default, except for "ref var", which was handled above.
        (_, _, _) ->
          (S.InFlow `fromMaybe` flowMode, S.CopyMech `fromMaybe` mechMode, S.ConstChange `fromMaybe` changeMode)

-- | Returns a modified store declaration with a default const change filled in.
fillStoreDeclarationModes :: S.StoreDeclaration -> S.StoreDeclaration
fillStoreDeclarationModes (S.StoreDeclaration changeMode typedIdent) =
  S.StoreDeclaration (changeMode <|> Just S.ConstChange) typedIdent

-- | Returns a modified function declaration with all optional values filled in.
-- Updates the parameters, return declaration, global imports and local declarations.
fillFunction :: S.FunctionDeclaration -> S.FunctionDeclaration
fillFunction (S.FunctionDeclaration name params retDecl globImps locals body) =
  S.FunctionDeclaration name filledParams filledRetDecl filledGlobImps filledLocals body
  where
    filledParams = fillParamModes <$> params
    filledRetDecl = fillStoreDeclarationModes retDecl
    filledGlobImps = fillGlobalImportModes <$> globImps
    filledLocals = fillStoreDeclarationModes <$> locals

-- | Returns a modified procedure declaration with all optional values filled in.
-- Updates the parameters, global imports and local declarations.
fillProcedure :: S.ProcedureDeclaration -> S.ProcedureDeclaration
fillProcedure (S.ProcedureDeclaration name params globImps locals body) =
  S.ProcedureDeclaration name filledParams filledGlobImps filledLocals body
  where
    filledParams = fillParamModes <$> params
    filledGlobImps = fillGlobalImportModes <$> globImps
    filledLocals = fillStoreDeclarationModes <$> locals
