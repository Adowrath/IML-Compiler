module Main where

import System.IO
import System.Environment

import IML.Token.Tokenizer
import IML.Token.Tokens
import IML.Parser.Parser
import IML.Parser.GeneralParser

import qualified IML.Parser.SyntaxTree    as Syntax

-- Argument Dispatcher
ignoreResult :: (a -> IO b) -> a -> IO ()
ignoreResult f a = do
  _ <- f a
  return ()

dispatch :: [(String, [String] -> IO ())]
dispatch = 
  [ 
    ("-t", ignoreResult tokenizeFile),
    ("-p", ignoreResult parseFile)
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
  