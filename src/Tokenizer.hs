module Tokenizer where

import Tokens

tokenize :: String -> TokenList
tokenize []                  = []
tokenize ('('          : xs) = LParen               : tokenize xs
tokenize (')'          : xs) = RParen               : tokenize xs
tokenize (','          : xs) = Comma                : tokenize xs
tokenize (';'          : xs) = Semicolon            : tokenize xs
tokenize (':':'='      : xs) = Becomes              : tokenize xs
tokenize (':'          : xs) = Colon                : tokenize xs
tokenize ('*'          : xs) = Operator Times       : tokenize xs
tokenize ('+'          : xs) = Operator Plus        : tokenize xs
tokenize ('-'          : xs) = Operator Minus       : tokenize xs
tokenize ('='          : xs) = Operator Tokens.EQ   : tokenize xs
tokenize ('/':'='      : xs) = Operator Tokens.NE   : tokenize xs
tokenize ('<':'='      : xs) = Operator Tokens.LTE  : tokenize xs
tokenize ('>':'='      : xs) = Operator Tokens.GTE  : tokenize xs
tokenize ('<'          : xs) = Operator Tokens.LT   : tokenize xs
tokenize ('>'          : xs) = Operator Tokens.GT   : tokenize xs
-- tokenize ('/':'\\'     : xs) = Opeartor And   : tokenize xs
-- tokenize ('\\':'/'     : xs) = Operator Or    : tokenize xs
-- tokenize ('/':'\\':'?' : xs) = Operator CAnd  : tokenize xs
-- tokenize ('\\':'/':'?' : xs) = Operator COr   : tokenize xs
