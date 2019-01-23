curry' :: ((a, b) -> c) -> a -> b -> c
curry' f x y = f(x, y)

uncurry' :: (a -> b -> c) -> (a, b) -> c
uncurry' g (x, y) = g x y
