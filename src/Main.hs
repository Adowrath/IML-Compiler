module Main where

import System.IO
import System.Environment

import Tokenizer
import Tokens

-- Argument Dispatcher
dispatch :: [(String, [String] -> IO ())]
dispatch = 
  [ ("-f", tokenizeFile)
  ]

tokenizeFile :: [String] -> IO ()
tokenizeFile p = do
  let s = head p
  contents <- readFile s
  putStrLn $ "We tokenized " ++ show contents ++ " as the following:"
  print (tokenize contents)

main :: IO ()
main = do
  putStrLn "-----------------------------------------------"
  putStrLn "- Welcome to the friendly IML Compiler suite. -"
  putStrLn "-        Written by Cyrill and Dominik        -"
  putStrLn "-----------------------------------------------"
  (command:args) <- getArgs
  let (Just action) = lookup command dispatch
  action args
