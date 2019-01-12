all' :: [a] -> (a -> Bool) -> Bool
all' [x] f = f x
all' (x:xs) f = if not (f x) then False else all' xs f
