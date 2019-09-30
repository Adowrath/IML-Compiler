module IML.Token.Tokenizer where

import Data.Char
import Data.Maybe (fromMaybe)
import IML.Token.Tokens

tokenize :: String -> TokenList
tokenize []                  = []
tokenize ('\t'         :  _) = error "We don't like tabs. Actually, we do, but eh."
tokenize ('/':'/'      : xs) = tokenize $ dropWhile ('\n' /=) xs -- ^ Comments!
tokenize (' '          : xs) = tokenize xs
tokenize ('\n'         : xs) = tokenize xs
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
-- tokenize ('/':'\\'     : xs) = Operator And   : tokenize xs
-- tokenize ('\\':'/'     : xs) = Operator Or    : tokenize xs
-- tokenize ('/':'\\':'?' : xs) = Operator CAnd  : tokenize xs
-- tokenize ('\\':'/':'?' : xs) = Operator COr   : tokenize xs

tokenize (c               : xs)
    | isAsciiDigit c         = tokenizeNumber $ c : xs
    | isIdentifierStart c    =
          keywordOrIdentifier (c : identRest) : tokenize restLine
        where
          (identRest, restLine) = span isIdentifierPart xs

-- This is the error case
tokenize xs = error ("Error parsing: " ++ show xs)

-- |
-- Turns a given name either into a keyword token
-- if it is defined in 'IML.Token.Tokens.keywordList',
-- else returns it as an identifier.
keywordOrIdentifier :: String -> Token
keywordOrIdentifier name = fromMaybe (Ident name) $ lookup name keywordList

tokenizeNumber :: String -> TokenList
tokenizeNumber str = IntLit (read number) : tokenize rest
  where
    (number, rest) = tokenizeInner True str

    {-
      If the boolean is true, i.e., we are directly after an apostrophe ('),
      it is an error for the number literal to just end, it must have at least
      one more digit after it.
    -}
    tokenizeInner :: Bool -> String -> (String, String)
    tokenizeInner True  []          = error "Tick at end of integer literal."
    tokenizeInner False []          = ([], [])
    tokenizeInner _     ('\'' : xs) = tokenizeInner True xs
    tokenizeInner post' (c    : xs)
      | isAsciiDigit c              = combine c $ tokenizeInner False xs
      | post'                       = error "Tick at end of integer literal."
      | otherwise                   = ("", c : xs)

    -- Just prepends the given character to the first string in the tuple.
    combine :: Char -> (String, String) -> (String, String)
    combine c (xs, r)              = (c : xs, r)

{-
    These are helper functions for the atoms of a token.
-}

isAsciiDigit :: Char -> Bool
isAsciiDigit c = '0' <= c && c <= '9'

isIdentifierStart :: Char -> Bool
isIdentifierStart c =
  'a' <= c && c <= 'z' ||
  'A' <= c && c <= 'Z'

isIdentifierPart :: Char -> Bool
isIdentifierPart c =
    isIdentifierStart c ||
    isAsciiDigit c ||
    c == '\'' ||
    c == '_'
