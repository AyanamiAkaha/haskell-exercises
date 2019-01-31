hex :: (Integral a) => a -> String
hex a
  | a < 0 = '-':hex (-a)
  | a < 16 = hexdigit (fromIntegral a :: Int)
  | otherwise = hex (a `div` 16) ++ hexdigit (a `mod` 16)
  where
    hexdigit :: (Integral n) => n -> String
    hexdigit n = ["0123456789abcdef" !! (fromIntegral n :: Int)] 
