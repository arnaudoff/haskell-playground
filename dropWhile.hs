dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' f (x:xs) =
    if f x then dropWhile' f xs else x : dropWhile' f xs
