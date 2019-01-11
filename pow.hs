pow' :: (Num a, Eq a) => a -> a -> a
pow' x n
    | n == 0 = 1
    | otherwise = x * pow' x (n - 1)
