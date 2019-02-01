module RPN (
  Token (..),
  Op (..),
  Calc (..),
  calc
  ) where

data Op = Add | Sub | Mul | Div deriving Show
data Token = Number Float | Operator Op deriving Show
data Calc = Calc [Token] | Result Float deriving Show

instance Read Op where
  readsPrec _ ('+':xs) = [(Add, xs)]
  readsPrec _ ('-':xs) = [(Sub, xs)]
  readsPrec _ ('*':xs) = [(Mul, xs)]
  readsPrec _ ('/':xs) = [(Div, xs)]

calc :: [Token] -> Calc
calc [] = Calc []
calc [a] = Calc [a]
calc [a,b] = Calc [a,b]
calc [Number x, Number y, Operator Add] = Result (x + y)
calc [Number x, Number y, Operator Sub] = Result (x - y)
calc [Number x, Number y, Operator Mul] = Result (x * y)
calc [Number x, Number y, Operator Div] = Result (x / y)
calc (a:xs) = calc' (a, calc xs)
  where
    calc' :: (Token, Calc) -> Calc
    calc' (x, Result r) = Calc [x, Number r]
    calc' (x, Calc c) = calc' (x, calc xs)
