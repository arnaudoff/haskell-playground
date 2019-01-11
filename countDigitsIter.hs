countDigits :: Int -> Int
countDigits n = loop 0 n
    where
        loop :: Int -> Int -> Int
        loop count n
          | n < 1 = count
          | otherwise = loop (count + 1) (n `div` 10)
