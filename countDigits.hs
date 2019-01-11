countDigits :: Int -> Int
countDigits n
  | n <= 9 = 1
  | otherwise = 1 + countDigits (n `div` 10)
