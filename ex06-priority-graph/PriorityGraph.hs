module PriorityGraph (
  PriorityGraph (..),
  setParent,
  addParent,
  priority
  ) where

type Priority = Double
type Parent a = (PriorityGraph a, Priority)
data PriorityGraph a = PriorityGraph (a, [Parent a]) | Root (a, Priority)
  deriving (Eq, Show)

-- TODO: switch to sets
-- TODO: prevent cycles
-- TODO: possibly make it a monad

setParent :: PriorityGraph a -> PriorityGraph a -> PriorityGraph a
setParent (Root (c, pc)) p = PriorityGraph (c, [(p, pc)])
-- TODO: setParent for PriorityGraph (replace parents)
setParent a _ = a -- dummy for now

addParent :: PriorityGraph a -> Parent a -> PriorityGraph a
addParent (PriorityGraph (t, [])) p = PriorityGraph (t, [p])
addParent (PriorityGraph (t, ps)) p = PriorityGraph (t, p:ps)
addParent (Root (t, _)) p = PriorityGraph (t, [p])

-- TODO: delete parent

priority' :: (PriorityGraph a, Priority) -> Priority
priority' (Root (_, pr), p) = p * pr
priority' (pp, p) = p * priority pp

priority :: PriorityGraph a -> Priority
priority (Root (_, p)) = p
priority (PriorityGraph (a, pp)) = sum $ map priority' pp
