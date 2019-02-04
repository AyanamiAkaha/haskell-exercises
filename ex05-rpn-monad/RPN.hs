module RPN (
  Token (..),
  RPN (..),
  ) where

data Token a = Number a | Operator a | BadInput
  deriving (Eq, Ord)

instance (Show a) => Show (Token a) where
  show (Number a) = show a
  show (Operator a) = show a
  show BadInput = "Bad Input"

data RPN a = RPN [Token a] deriving (Ord, Eq, Show)

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
