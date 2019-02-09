module PriorityGraph (
  PriorityGraph (..),
  addParent
  ) where

type Priority = Double
type Parent a = (PriorityGraph a, Priority)
data PriorityGraph a = PriorityGraph (a, [Parent a]) | Root (a, Priority)
  deriving (Eq, Show)

addParent :: PriorityGraph a -> PriorityGraph a -> PriorityGraph a
addParent (Root (c, pc)) p =
  PriorityGraph (c, [(p, pc)])
addParent a _ = a -- dummy for now

