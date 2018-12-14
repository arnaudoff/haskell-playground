quickSort :: [Int] -> [Int]
quickSort [] = []
quickSort (pivot:xs) =
    quickSort [x | x <- xs, x <= pivot] ++ [pivot] ++ quickSort [y | y <- xs, y > pivot]
