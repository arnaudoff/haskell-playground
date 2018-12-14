insert :: Int -> [Int] -> [Int]
insert x [] = [x]
insert x (y:ys)
    | x <= y = x : (y:ys)
    | otherwise = y : insert x ys

insertSort :: [Int] -> [Int]
insertSort [] = []
insertSort (x:xs) = insert x (insertSort xs)
