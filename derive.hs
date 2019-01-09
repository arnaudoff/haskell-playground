derive :: (Double -> Double) -> Double -> (Double -> Double)
derive f dx = \x -> (f (x + dx) - f x) / dx
