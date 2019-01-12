binToDec :: Int -> Int
binToDec n = loop n 0 0
    where
        loop :: Int -> Int -> Int -> Int
        loop n power result =
            if n < 1 then result else loop (n `div` 10) (power + 1) (result + ((n `mod` 10) * 2 ^ power))
