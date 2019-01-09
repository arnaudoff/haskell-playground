derive :: (Double -> Double) -> Double -> (Double -> Double)
derive f dx = \x -> (f (x + dx) - f x) / dx

nthDerive :: (Double -> Double) -> Int -> Double -> (Double -> Double)
nthDerive f n dx
    | n == 1 = derive f dx
    | n > 1 = derive (nthDerive f (n - 1) dx) dx
