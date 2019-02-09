module PriorityGraph (
  PriorityGraph (..),
  setParent,
  addParent,
  priority,
  getTasks
  ) where

type Priority = Double
type Parent a = (PriorityGraph a, Priority)
data PriorityGraph a = PriorityGraph (a, [Parent a]) | Root (a, Priority)
  deriving (Eq, Show)

mapParent' :: (a -> b) -> (PriorityGraph a, Priority) -> (PriorityGraph b, Priority)
mapParent' f (Root (a, pr), p) = (Root ((f a), pr), p)
mapParent' f (g, p) = (fmap f g, p)

mapParent :: (a -> b) -> [(PriorityGraph a, Priority)] -> [(PriorityGraph b, Priority)]
mapParent f [] = []
mapParent f xs = map (mapParent' f) xs

getTasks' :: [Parent a] -> [a]
getTasks' [] = []
getTasks' ((t,_):pp) = (getTasks t) ++ (getTasks' pp)

getTasks :: PriorityGraph a -> [a]
getTasks (Root (t, _)) = [t]
getTasks (PriorityGraph (t, ps)) = t:(getTasks' ps)

instance Functor PriorityGraph where
  fmap f (Root (t, p)) = Root (f t, p)
  fmap f (PriorityGraph (t, pp)) = PriorityGraph (f t, mapParent f pp)

instance Applicative PriorityGraph where
  pure a = Root (a, 1)
  Root (f, p1) <*> pg = fmap f pg
--  PriorityGraph (f, fs) <*> PriorityGraph (t, ts) = _

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
