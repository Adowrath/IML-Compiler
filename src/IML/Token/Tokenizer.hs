module IML.Token.Tokenizer where

import IML.Token.Tokens

tokenize :: String -> TokenList
tokenize []                  = []
tokenize (' '          : xs) = tokenize xs
tokenize ('\n'         : xs) = tokenize xs
tokenize ('\f'         : xs) = tokenize xs
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

tokenize (c : xs)
    | possibleKeyWord c = keywordOrIdent c xs




tokenize xs = error xs
-- tokenize ('/':'\\'     : xs) = Opeartor And   : tokenize xs
-- tokenize ('\\':'/'     : xs) = Operator Or    : tokenize xs
-- tokenize ('/':'\\':'?' : xs) = Operator CAnd  : tokenize xs
-- tokenize ('\\':'/':'?' : xs) = Operator COr   : tokenize xs


possibleKeyWord :: Char -> Bool
possibleKeyWord c = ( ('a' <=  c && c <= 'z') || ('A' <=  c && c <= 'Z') )

isDigit :: Char -> Bool
isDigit c = ( '0' <= c && c <= '9' )

keywordOrIdent :: Char -> String -> TokenList
keywordOrIdent c xs = undefined 
    where   (t1, t2) = span isPartOfIdentifier xs
            name = c : t1
            Token = case lookup name keywordList of (Just kwToken) -> 
            

isPartOfIdentifier :: Char -> Bool
isPartOfIdentifier c = ( possibleKeyWord c || (elem c ['\'', '_']) || isDigit c )