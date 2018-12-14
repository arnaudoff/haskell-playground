sumPairs :: [(Int, Int)] -> [Int]
sumPairs xs = [ x+y | (x, y) <- xs]

sumMonPairs :: [(Int, Int)] -> [Int]
sumMonPairs xs = [ x + y | (x, y) <- xs, x <= y]
