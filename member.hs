member' :: (Eq a) => [a] -> a -> Bool
member' [] _ = False
member' (x:xs) n = if x == n then True else member' xs n
