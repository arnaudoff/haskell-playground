moreLists :: [a] -> [a] -> [[a]]
moreLists [] [] = []
moreLists (x:xs) (y:ys) = [x, y] : moreLists xs ys
