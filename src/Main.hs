module Main where

import System.IO
import System.Environment

import Tokenizer
import Tokens

-- Argument Dispatcher
dispatch :: [(String, [String] -> IO ())]
dispatch = [ ("-f", file) 
            ]


file :: [String] -> IO ()
file p = do
  let 
    s = head p
  contents <- readFile s
  putStrLn $ show $ tokenize contents

main :: IO ()
main = do
  putStrLn "hello world"
  (command:args) <- getArgs
  let (Just action) = lookup command dispatch
  action args