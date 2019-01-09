after' :: (b -> c) -> (a -> b) -> (a -> c)
after' f g = \x -> f (g x)
