countOccurances :: (Eq a) => [a] -> a -> Int
countOccurances [] y = 0
countOccurances (x:xs) y =
    if x == y then 1 + countOccurances xs y else countOccurances xs y
