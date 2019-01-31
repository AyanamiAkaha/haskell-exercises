import Debug.Trace

isPrime :: (Integral a) => a -> (Maybe a, Bool)
isPrime n
  | n == 0 = (Nothing, True)
  | n == 1 = (Nothing, True)
  | otherwise = isPrimePartial n 2
  where
    isPrimePartial :: (Integral a) => a -> a -> (Maybe a, Bool)
    isPrimePartial n dv
      | dv*dv > n = (Nothing, True)
      | n `mod` dv == 0 = (Just dv, False)
      | otherwise = isPrimePartial n (dv+1)

nextPrime :: (Integral a) => a -> a
nextPrime n
  | check (isPrime n) = n
  | otherwise = nextPrime (n+1)
  where
    check :: (Maybe a, Bool) -> Bool
    check (_, b) = b

isPrime' :: (Integral a) => a -> Bool
isPrime' n
  | n < 0 = isPrime' (-n)
  | n == 0 = True
  | n == 1 = True
  | otherwise = (head . filter p $ [sqrtn, sqrtn-1..]) == 1
  where
    sqrtn = floor . sqrt . fromIntegral $ n
    p x = n `mod` x == 0
