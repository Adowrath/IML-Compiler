module IML.Token.Tokenizer where

import IML.Token.Tokens

tokenize :: String -> TokenList
tokenize []                  = []
tokenize ('('          : xs) = LParen                      : tokenize xs
tokenize (')'          : xs) = RParen                      : tokenize xs
tokenize (','          : xs) = Comma                       : tokenize xs
tokenize (';'          : xs) = Semicolon                   : tokenize xs
tokenize (':':'='      : xs) = Becomes                     : tokenize xs
tokenize (':'          : xs) = Colon                       : tokenize xs
tokenize ('*'          : xs) = Operator Times              : tokenize xs
tokenize ('+'          : xs) = Operator Plus               : tokenize xs
tokenize ('-'          : xs) = Operator Minus              : tokenize xs
tokenize ('='          : xs) = Operator Equals             : tokenize xs
tokenize ('/':'='      : xs) = Operator NotEquals          : tokenize xs
tokenize ('<':'='      : xs) = Operator LessThanEquals     : tokenize xs
tokenize ('>':'='      : xs) = Operator GreaterThanEquals  : tokenize xs
tokenize ('<'          : xs) = Operator LessThan           : tokenize xs
tokenize ('>'          : xs) = Operator GreaterThan        : tokenize xs
-- tokenize ('/':'\\'     : xs) = Opeartor And   : tokenize xs
-- tokenize ('\\':'/'     : xs) = Operator Or    : tokenize xs
-- tokenize ('/':'\\':'?' : xs) = Operator CAnd  : tokenize xs
-- tokenize ('\\':'/':'?' : xs) = Operator COr   : tokenize xs
