matrixDiagonal :: [[a]] -> [a]
matrixDiagonal xxs = loop xxs ((length xxs) - 1) 0 []
    where
        loop :: [[a]] -> Int -> Int -> [a] -> [a]
        loop [] _ _ result = reverse result
        loop (xs:xxs) len current result
            | current > len = []
            | otherwise = loop xxs len (current + 1) ((xs !! current) : result)
