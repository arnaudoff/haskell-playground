countDigits :: Int -> Int
countDigits n
  | n <= 9 = 1
  | otherwise = 1 + countDigits (n `div` 10)

loop :: Int -> Int -> Int -> Int
loop n digitsLeft result
  | digitsLeft == 0 = result
  | otherwise = loop (n `div` 10) (digitsLeft - 1) (result + n `mod` 10 * 10 ^ (digitsLeft - 1))

reverseDigits :: Int -> Int
reverseDigits n = loop n (countDigits n) 0

palindrome :: Int -> Bool
palindrome n = n == reverseDigits n
