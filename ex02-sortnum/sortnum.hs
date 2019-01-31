import Text.Read

type Dirty = Bool

bSortStep :: (Ord a) => [a] -> (Dirty, [a])
bSortStep [] = (False, [])
bSortStep [a] = (False, [a])
bSortStep (x:y:xs) = (dirty || dirty', a:xs')
  where
    (dirty, a, b) = if x>y then (True, y, x) else (False, x, y)
    (dirty', xs') = bSortStep (b:xs)

bubbleSort :: (Ord a) => [a] -> [a]
bubbleSort [] = []
bubbleSort [a] = [a]
bubbleSort xs = if dirty then bubbleSort xs' else xs'
  where (dirty, xs') = bSortStep xs
  
strArrayToFloat :: [String] -> [Float]
strArrayToFloat [] = []
strArrayToFloat (x:xs) = (extract . readMaybe $ x :: [Float]) ++ (strArrayToFloat xs)
  where
    extract (Just v) = [v]
    extract Nothing = []

-- use words in case there are multiple numbers in one line, but unlines
-- for easier reading
main = interact $ unlines . map show . bubbleSort . strArrayToFloat . words
