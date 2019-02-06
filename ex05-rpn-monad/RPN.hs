module RPN (
  RPN (..),
  rpn
  ) where

import Token

data RPN a = RPN [Token a] deriving (Eq, Show)

instance Functor RPN where
  fmap _ _ = RPN [BadInput]

instance Applicative RPN where
  pure _ = RPN [BadInput]
  _ <*> _ = RPN [BadInput]

instance Monad RPN where
  return a = RPN [BadInput]
  _ >>= _ = RPN [BadInput]

rpn :: String -> RPN Float
rpn s =  RPN (map tparse $ words s)
