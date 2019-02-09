module PriorityGraph (
  PriorityGraph (..),
  setParent,
  addParent
  ) where

type Priority = Double
type Parent a = (PriorityGraph a, Priority)
data PriorityGraph a = PriorityGraph (a, [Parent a]) | Root (a, Priority)
  deriving (Eq, Show)

setParent :: PriorityGraph a -> PriorityGraph a -> PriorityGraph a
setParent (Root (c, pc)) p =
  PriorityGraph (c, [(p, pc)])
setParent a _ = a -- dummy for now

addParent :: PriorityGraph a -> Parent a -> PriorityGraph a
addParent (PriorityGraph (t, [])) p = PriorityGraph (t, [p])
addParent (PriorityGraph (t, ps)) p = PriorityGraph (t, p:ps)
addParent (Root (t, _)) p = PriorityGraph (t, [p])
