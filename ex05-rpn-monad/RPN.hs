module RPN (
  Token (..),
  RPN (..),
  ) where

import Token

data RPN a = RPN [Token a] deriving (Eq, Show)

instance Functor RPN where
  fmap _ _ = RPN [BadInput]

instance Applicative RPN where
  pure = makeRPN
  _ <*> _ = RPN [BadInput]

instance Monad RPN where
  return = makeRPN
  _ >>= _ = RPN []

makeRPN :: a -> RPN a
makeRPN _ = RPN [BadInput]
