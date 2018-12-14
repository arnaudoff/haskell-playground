drop' :: [a] -> Int -> [a]
drop' [] _ = []
drop' xs n = if n == 0 then xs else drop' (tail xs) (n - 1)
