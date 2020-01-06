module Main where

import System.IO
import System.Environment

import IML.Token.Tokenizer
import IML.Token.Tokens
import IML.Parser.Parser
import IML.Parser.GeneralParser
import IML.CodeGen.TypeCheck
import IML.CodeGen.DefaultsPhase
import IML.CodeGen.CodeGen

import qualified IML.Parser.SyntaxTree    as Syntax
import qualified VirtualMachineIO         as VM
import qualified Locations
import qualified BaseDecls

-- Argument Dispatcher
ignoreResult :: (a -> IO b) -> a -> IO ()
ignoreResult f a = do
  _ <- f a
  return ()

dispatch :: [(String, [String] -> IO ())]
dispatch = 
  [ 
    ("-t", ignoreResult tokenizeFile),
    ("-p", ignoreResult parseFile),
    ("-d", ignoreResult defPhase),
    ("-c", ignoreResult checkParsed),
    ("-g", ignoreResult codeGen),
    ("-v", ignoreResult execVM)
  ]

tokenizeFile :: [String] -> IO [Token]
tokenizeFile p = do
  let s = head p
  contents <- readFile s
  putStrLn $ "We tokenized " ++ show contents ++ " as the following:"
  let tokens = tokenize contents
  print tokens
  return tokens

parseFile :: [String] -> IO Syntax.Program
parseFile p = do
  tokens <- tokenizeFile p
  putStrLn "We paresed it as the following:"
  let syntax_tree = parser tokens
  print syntax_tree
  return syntax_tree

parser :: [Token] -> Syntax.Program
parser toks =
  case parse parseProgram toks of
    [(prog, [])] -> prog
    [_]          -> error "internal error"
    []           -> error "syntax error"

defPhase :: [String] -> IO Syntax.Program
defPhase p = do
    syntax_tree <- parseFile p
    putStrLn "We filled defaults as the following:"
    let filled_syntax_tree = fillProgram syntax_tree
    print filled_syntax_tree
    return filled_syntax_tree

checkParsed :: [String] -> IO Syntax.Program
checkParsed p = do
    syntax_tree <- defPhase p
    putStrLn "We checked it as the following:"
    let checked_syntax_tree = typeChecks syntax_tree
    print checked_syntax_tree
    return checked_syntax_tree

codeGen :: [String] -> IO VM.VMProgram
codeGen p = do
    syntax_tree <- checkParsed p
    putStrLn "We generated the following VM-Code:"
    let vmP = compileProgramm syntax_tree
    print vmP
    return vmP

execVM :: [String] -> IO (Locations.Check BaseDecls.BaseIdent)
execVM p = do
    vmP <- codeGen p
    VM.execProgram vmP


-- TODO remove
compileProgramm :: Syntax.Program -> VM.VMProgram
compileProgramm = undefined

main :: IO ()
main = do
  putStrLn "-----------------------------------------------"
  putStrLn "- Welcome to the friendly IML Compiler suite! -"
  putStrLn "-        Written by Cyrill and Dominik        -"
  putStrLn "-----------------------------------------------"
  (command:args) <- getArgs
  case lookup command dispatch of
    Just action -> action args
    Nothing -> print $ "Arguments not recognized: " ++ command ++ show args
  