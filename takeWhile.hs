takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' f [] = []
takeWhile' f (x:xs) =
    if (f x) then x : takeWhile' f xs else takeWhile' f xs
