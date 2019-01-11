sin' :: Double -> Int -> Double
sin' x n
    | n == 0 = sin 1
    | otherwise = sin (x ^ n) + sin' x (n - 1)
