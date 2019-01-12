compose' :: (Num a, Ord a) => (a -> a) -> a -> (a -> a)
compose' f n
    | n <= 1 = \x -> f x
    | otherwise = \x -> f (compose' f (n - 1) x)
