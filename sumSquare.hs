sumSquare :: (Num a) => [a] -> a
sumSquare xs = (foldr (+) 0 (map (^2) xs)) * 2
