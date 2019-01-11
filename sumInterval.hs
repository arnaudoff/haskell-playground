sumInterval :: (Integral a) => a -> a -> a
sumInterval x y
    | x > y = 0
    | otherwise = x + sumInterval (x + 1) y
