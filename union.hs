union :: (Eq a) => [a] -> [a] -> [a]
union xs ys = loop xs ys []

loop :: (Eq a) => [a] -> [a] -> [a] -> [a]
loop [] [] result = result
loop (x:xs) ys result
    | x `elem` result = loop xs ys result
    | otherwise = loop xs ys (x : result)
loop [] (y:ys) result
    | y `elem` result = loop [] ys result
    | otherwise = loop [] ys (y : result)

