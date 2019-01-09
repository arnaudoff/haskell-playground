chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
    | even n = n : chain (n `div` 2)
    | odd n = n : chain (n * 3 + 1)

countChainsLongerThan :: Int -> Int -> Int -> Int
countChainsLongerThan x s e = length (filter (\y -> length y > x) (map chain [s..e]))
