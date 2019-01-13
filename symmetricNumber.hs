symmetricNumber :: (Integral a) => a -> a
symmetricNumber n = loop n ((countDigits n) - 1) 0
    where
        countDigits :: (Integral a) => a -> a
        countDigits n
          | n < 1 = 0
          | otherwise = 1 + countDigits (n `div` 10)
        loop :: (Integral a) => a -> a -> a -> a
        loop n power result
            | n < 1 = result
            | otherwise = loop (n `div` 10) (power - 1) (result + (n `mod` 10) * (10 ^ power))
