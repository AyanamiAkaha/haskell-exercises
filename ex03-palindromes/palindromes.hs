main = interact $ show . length . filter (\w -> w == reverse w) . words
