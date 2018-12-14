fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibStep :: (Int, Int) -> (Int, Int)
fibStep (u, v) = (v, u + v)

fibPair :: Int -> (Int, Int)
fibPair n
    | n == 0    = (0, 1)
    | otherwise = fibStep (fibPair (n - 1))
