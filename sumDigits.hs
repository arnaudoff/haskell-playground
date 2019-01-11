sumDigits :: Int -> Int
sumDigits n
  | n <= 9 = n
  | otherwise = n `mod` 10 + sumDigits (n `div` 10)
