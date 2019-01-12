sumDigitsIter :: Int -> Int
sumDigitsIter n = loop 0 n
    where
        loop :: Int -> Int -> Int
        loop sum n =
            if n < 1 then sum else loop (sum + (n `mod` 10)) (n `div` 10)
