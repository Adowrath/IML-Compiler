-- IML, HS-2012, Ruedi
-- Edgar F.A. Lederer, FHNW

module ErrorHandlingGeneric where

import Data.Monoid

class (Eq l, Monoid l) => Loc l where
  noLoc :: l
  noLoc = Data.Monoid.mempty
  (<>) :: l -> l -> l
  (<>) = (Data.Monoid.<>)

newtype ErrorMsgGen l = ErrorMsg ([l], String)
  deriving (Show)

type CheckGen l a = Either (ErrorMsgGen l) a

type CheckerGen l a b = a -> CheckGen l b

--instance Monad (Either a) where
--  return x = Right x
--  Right x >>= f = f x
--  Left y >>= _ = Left y
