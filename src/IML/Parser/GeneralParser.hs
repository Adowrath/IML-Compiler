{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase   #-}

module IML.Parser.GeneralParser where

import           Control.Applicative

-- |
-- General parser defined over a component type 't'
-- parsing a value of type 'a'.
newtype GenParser t a =
  P ([t] -> [(a, [t])])

instance Functor (GenParser t) where
  fmap :: (a -> b) -> GenParser t a -> GenParser t b
  fmap f pA =
    P $ \tokens ->
      case parse pA tokens of
        []        -> []
        [(a, as)] -> [(f a, as)]

instance Applicative (GenParser t) where
  pure :: a -> GenParser t a
  pure a = P $ \tokens -> [(a, tokens)]
  (<*>) :: GenParser t (a -> b) -> GenParser t a -> GenParser t b
  pF <*> pA =
    P $ \tokens ->
      case parse pF tokens of
        []        -> []
        [(f, fs)] -> parse (fmap f pA) fs

instance Monad (GenParser t) where
  (>>=) :: GenParser t a -> (a -> GenParser t b) -> GenParser t b
  pA >>= f =
    P $ \tokens ->
      case parse pA tokens of
        []        -> []
        [(a, as)] -> parse (f a) as

instance Alternative (GenParser t) where
  empty :: GenParser t a
  empty = P $ const []
  (<|>) :: GenParser t a -> GenParser t a -> GenParser t a
  p1 <|> p2 =
    P $ \tokens ->
      case parse p1 tokens of
        []      -> parse p2 tokens
        res@[_] -> res

-- | Executes a parser on a given input.
parse :: GenParser t a -> [t] -> [(a, [t])]
parse (P parser) = parser

-- | Just takes one element.
item :: GenParser t t
item =
  P $ \case
    [] -> []
    (x:xs) -> [(x, xs)]

-- | Takes one element, only if it matches the predicate, else fails.
satisfies :: (t -> Bool) -> GenParser t t
satisfies p = do
  x <- item
  if p x
    then return x
    else empty

-- | Takes 0..* elements separated by the given token.
manySep :: Eq t => t -> GenParser t a -> GenParser t [a]
manySep separator parseA = ((:) <$> parseA <*> parseSepA) <|> return []
  where
    parseSepA = many (terminal separator >> parseA)
    --  Parse 'a's preceded by the separator, ignoring it.

-- | Takes 1..* elements separated by the given token.
someSep :: Eq t => t -> GenParser t a -> GenParser t [a]
someSep separator parseA = (:) <$> parseA <*> parseSepA
  where
    parseSepA = many (terminal separator >> parseA)
    --  Parse 'a's preceded by the separator, ignoring it.

-- | Takes one element if it equals the given element.
terminal :: Eq t => t -> GenParser t t
terminal = satisfies . (==)
