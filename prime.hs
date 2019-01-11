prime' :: Int -> Bool
prime' x = loop 2 x
    where
        loop :: Int -> Int -> Bool
        loop current n
          | current >= n = True
          | n `mod` current == 0 = False
          | otherwise = loop (current + 1) n
