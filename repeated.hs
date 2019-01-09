repeated :: (a -> a) -> Int -> (a -> a)
repeated f n
    | n == 1    = f
    | otherwise = \x -> f (repeated f (n - 1) x)

repeated' :: (a -> a) -> Int -> (a -> a)
repeated' f n
    | n == 1 = f
    | otherwise = f . repeated' f (n - 1)
