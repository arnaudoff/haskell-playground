append' :: [a] -> [a] -> [a]
append' xs ys = loop xs ys []
    where loop :: [a] -> [a] -> [a] -> [a]
          loop (x:xs) ys result = loop xs ys (x : result)
          loop [] (y:ys) result = loop [] ys (y : result)
          loop [] [] result = reverse result
