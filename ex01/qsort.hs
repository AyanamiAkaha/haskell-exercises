qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort [n] = [n]
qsort (x:xs) =
  let l = qsort [n | n <- xs, n <= x]
      r = qsort [n | n <- xs, n <= x]
   in l ++ [x] ++ r
