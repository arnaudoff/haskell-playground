reverse' :: [Int] -> [Int]
reverse' [] = []
reverse' (x:xs) = reverse xs ++ [x]
