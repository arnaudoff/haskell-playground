pythagTriples :: [(Int, Int, Int)]
pythagTriples = [(x, y, z) | z <- [3..], y <- [2..z-1], x <- [2..y-1], x * x + y * y == z * z]
