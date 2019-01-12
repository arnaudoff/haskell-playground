skipMatrixRow :: [[a]] -> Int -> [[a]]
skipMatrixRow xxs n = loop n xxs 0 []
    where
        loop :: Int -> [[a]] -> Int -> [[a]] -> [[a]]
        loop _ [] _ result = reverse result
        loop n (xs:xss) i result =
            if i == n then loop n xxs (i + 1) result else loop n xxs (i + 1) (xs : result)
