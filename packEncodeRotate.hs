pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack (x:xs) = (x : takeWhile (\y -> y == x) xs) : pack (dropWhile (\y -> y == x) xs)

encode :: (Eq a) => [a] -> [(Int, a)]
encode [] = []
encode (x:xs) = (length (x : takeWhile (\y -> y == x) xs), x) : encode (dropWhile (\y -> y == x) xs)

rotate :: (Enum a) => [a] -> Int -> [a]
rotate xs n
    | n < 0 = rotate xs (n + len)
    | n > len = rotate xs (n - len)
    | otherwise = let (first, second) = splitAt n xs in second ++ first
    where len = length xs
