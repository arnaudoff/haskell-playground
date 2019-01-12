nthMatrixColumn :: [[a]] -> Int -> [a]
nthMatrixColumn [] n = []
nthMatrixColumn (x:xxs) n = (x !! n) : nthMatrixColumn xxs n
