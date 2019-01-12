any' :: [a] -> (a -> Bool) -> Bool
any' [] f = False
any' (x:xs) f = if f x then True else any' xs f
