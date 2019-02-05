module Token (
  Token (Op, BadInput),
  Operator (Add, Sub, Mul, Div),
  tnum,
  tparse
 ) where

import Text.Read

data Operator = Add | Sub | Mul | Div
    deriving (Show, Eq, Read)

data Token a = Num a | Op Operator | BadInput
  deriving Eq

tnum :: (Num a) => a -> Token a
tnum a = Num a

instance (Show a) => Show (Token a) where
  show (Num n) = show n
  show (Op op) = show op
  show BadInput = "Bad Input"

tparse :: String -> Token Float
tparse "add" = Op Add
tparse "sub" = Op Sub
tparse "mul" = Op Mul
tparse "div" = Op Div
tparse a = extract v
   where
     v = (readMaybe a :: Maybe Float)
     extract :: Maybe Float -> Token Float
     extract (Just a) = Num a
     extract _ = BadInput

