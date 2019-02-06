module RPN (
  RPN (..),
  rpn
  ) where

import Token

data RPN a = RPN [Token a] deriving (Eq, Show)

rpn :: String -> RPN Float
rpn s = RPN (map tparse $ words s)
