filterEven :: [Int] -> [Int]
filterEven [] = []
filterEven (x:xs)
    | even x    = x : filterEven xs
    | otherwise = filterEven xs

filterEven' :: [Int] -> [Int]
filterEven' xs = [x | x <- xs, even x]
