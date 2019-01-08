until' :: (a -> Bool) -> (a -> a) -> a -> a
until' b f x
    | b x = x
    | otherwise = until b f (f x)

