reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse xs ++ [x]

reverse'' :: [a] -> [a]
reverse'' xs = loop xs []
    where
        loop [] ys = ys
        loop (x:xs) ys = loop xs (x:ys)
