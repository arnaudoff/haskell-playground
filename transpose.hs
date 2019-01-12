nthMatrixColumn :: [[a]] -> Int -> [a]
nthMatrixColumn [] n = []
nthMatrixColumn (x:xxs) n = (x !! n) : nthMatrixColumn xxs n

transpose' :: [[a]] -> [[a]]
transpose' xxs = loop xxs (length (head xxs)) []
    where
        loop :: [[a]] -> Int -> [[a]] -> [[a]]
        loop xxs cols result
            | cols <= 0 = result
            | otherwise = loop xxs (cols - 1) ((nthMatrixColumn xxs (cols - 1)) : result)

