variations :: (Num a, Eq a) => [a] -> a -> [[a]]
variations xs k = variate xs xs k
    where
        variate :: (Num a, Eq a) => [a] -> [a] -> a -> [[a]]
        variate _ [] _ = []
        variate ys xs 1 = map (\x -> [x]) xs
        variate ys (x:xs) k =
            (map (\ys -> x:ys) (variate ys (filter (\y -> y /= x) ys) (k - 1))) ++ variate ys xs k
