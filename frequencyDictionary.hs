freqDictOrdered :: [Int] -> [Int] -> [[Int]]
freqDictOrdered xs ys = quicksort (freqDict xs ys) (\x y -> (x !! 1) <= (y !! 1))

freqDict :: [Int] -> [Int] -> [[Int]]
freqDict [] _ = []
freqDict (x:xs) ys = [x, length (filter (\y -> y == x) ys)] : freqDict (filter (/=x) xs) ys

quicksort :: (Ord a) => [a] -> (a -> a -> Bool) -> [a]
quicksort [] f = []
quicksort (x:xs) f = (quicksort smaller f) ++ [x] ++ (quicksort larger f)
    where smaller = [y | y <- xs, f x y]
          larger = [y | y <- xs, not (f x y)]
