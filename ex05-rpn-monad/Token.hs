module Token (
  Token (..),
  Operator (..),
  parse
 ) where

import Text.Read

data Operator = Add | Sub | Mul | Div
    deriving (Show, Eq, Read)

data Token = Num Float | Op Operator | BadInput
  deriving Eq

instance Show Token where
  show (Num n) = show n
  show (Op op) = show op
  show BadInput = "Bad Input"

parse :: String -> Token
parse "add" = Op Add
parse "sub" = Op Sub
parse "mul" = Op Mul
parse "div" = Op Div
parse a = extract v
  where
    v = (readMaybe a :: Maybe Float)
    extract :: Maybe Float -> Token
    extract (Just a) = Num a
    extract _ = BadInput

