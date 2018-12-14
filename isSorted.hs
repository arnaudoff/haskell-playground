isSorted :: (Ord a) => [a] -> Bool
isSorted [] = True
isSorted [x] = True
isSorted (x : y : rst) = if x >= y then False else isSorted (y : ys)
