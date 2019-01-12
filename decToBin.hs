loop :: Int -> Int -> Int -> Int
loop n index result
    | n < 1 = result
    | otherwise =
        loop (n `div` 2) (index + 1) (result + (n `mod` 2 * (10 ^ index)))

decToBin :: Int -> Int
decToBin n = loop n 0 0

