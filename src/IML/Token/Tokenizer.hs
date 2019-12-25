module IML.Token.Tokenizer where

import Data.Maybe (fromMaybe)
import Text.Printf
import IML.Token.Tokens

tokenize :: String -> [Token]
tokenize []                  = []
tokenize ('\t'         :  _) = error "We don't like tabs. Actually, we do, but eh."
tokenize ('/':'/'      : xs) = tokenize $ dropWhile ('\n' /=) xs -- Comments!
tokenize (' '          : xs) = tokenize xs
tokenize ('\n'         : xs) = tokenize xs
tokenize ('('          : xs) = LParen                      : tokenize xs
tokenize (')'          : xs) = RParen                      : tokenize xs
tokenize (','          : xs) = Comma                       : tokenize xs
tokenize (';'          : xs) = Semicolon                   : tokenize xs
tokenize (':':'='      : xs) = Becomes                     : tokenize xs
tokenize ('?'          : xs) = CondOpr                     : tokenize xs
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
tokenize ('/':'\\':'?' : xs) = Operator CAnd  : tokenize xs
tokenize ('\\':'/':'?' : xs) = Operator COr   : tokenize xs

tokenize (c               : xs)
    | isAsciiDigit c         = tokenizeNumber $ c : xs
    | isIdentifierStart c    =
          keywordOrIdentifier (c : identRest) : tokenize restLine
        where
          (identRest, restLine) = span isIdentifierPart xs

-- This is the error case
tokenize xs = error $ printf "Unexpected token: %c, rest of string was: \n%s" (head xs) xs

-- |
-- Turns a given name either into a keyword token
-- if it is defined in 'IML.Token.Tokens.keywordList',
-- else returns it as an identifier.
keywordOrIdentifier :: String -> Token
keywordOrIdentifier name = fromMaybe (Ident name) $ lookup name keywordList

tokenizeNumber :: String -> [Token]
tokenizeNumber str = IntLit (read number) : tokenize rest
  where
    (number, rest) = tokenizeInner True str

    {-
      If the boolean is true, i.e., we are directly after an apostrophe ('),
      it is an error for the number literal to just end, it must have at least
      one more digit after it.
    -}
    tokenizeInner :: Bool -> String -> (String, String)

    -- We are at the end of input, after a '.
    tokenizeInner True  []          = error "Tick at end of integer literal."
    -- We are at the end of input, not after a '.
    tokenizeInner False []          = ([], [])
    -- We have a ', just skip it and mark it as "after tick".
    tokenizeInner _     ('\'' : xs) = tokenizeInner True xs

    tokenizeInner post' (c    : xs)
    -- We are not at the end, but there's a digit. We take it.
      | isAsciiDigit c              = combine c $ tokenizeInner False xs
    -- We are after a ', but there isn't a ' or a digit. Invalid!
      | post'                       = error "Tick at end of integer literal."
    -- We do not have another digit or ', but we're after a '. We can end the literal.
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
