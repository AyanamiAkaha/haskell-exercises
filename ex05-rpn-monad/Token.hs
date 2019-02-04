module Token (
  Token (..),
  Operator (..)
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

instance Read Token where
  readsPrec _ "add" = [(Op Add, "")]
  readsPrec _ "sub" = [(Op Sub, "")]
  readsPrec _ "mul" = [(Op Mul, "")]
  readsPrec _ "div" = [(Op Div, "")]
  readsPrec _ a = [(extract v, "")]
    where
      v = (readMaybe a :: Maybe Float)
      extract :: Maybe Float -> Token
      extract (Just a) = Num a
      extract _ = BadInput

