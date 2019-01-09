add :: Int -> Int -> Int
add x y = x + y

addu :: (Int, Int) -> Int
addu (x, y) = x + y

square :: Int -> Int
square x = x * x

inc :: Int -> Int
inc = add 1

max' :: Int -> Int -> Int
max' x y
    | x >= y = x
    | otherwise = y

max'' :: Int -> Int -> Int
max'' x y = if x >= y then x else y

maxthreeu :: (Int, Int, Int) -> Int
maxthreeu (x, y, z)
  | x >= y && x >= z = x
  | y >= z = y
  | otherwise = z

f :: Float -> Float -> Float
f x y = a^3 + b^2*a - b^3*x*y
    where
        a = 1 + x^4 * y^3
        b = x^2 + y^3

solvequadratic :: Float -> Float -> Float -> [Float]
solvequadratic a b c
    | d < 0 = []
    | otherwise = [(-b + sqrt d)/(2*a), (-b - sqrt d) / (2 * a)]
    where
        d = b * b - 4 * a * c

max_cube :: Float -> Float -> Float
max_cube x y
    | cube_x > cube_y = cube_x
    | otherwise = cube_y
  where
      cube_x = cube x
      cube_y = cube y
      cube :: Float -> Float
      cube z = z * z * z

sqlet :: Int -> Int
sqlet x = let y = x in y * y

solvequadraticlet :: Float -> Float -> Float -> [Float]
solvequadraticlet a b c =
    let d = b * b - 4 * a * c in
        if d < 0 then [] else [(-b + sqrt d)/(2*a), (-b - sqrt d)/(2*a)]

sumofsq :: Int -> Int -> Int
sumofsq x y = sqx + sqy
    where
        sqx = sqr x
        sqy = sqr y

        sqr :: Int -> Int
        sqr x = x * x

plus3 :: [Int] -> [Int]
plus3 xs = map (+3) xs

oddSquareSum :: Integer
oddSquareSum =
    let oddSquares = filter odd $ map (^2) [1..]
        belowLimit = takeWhile (<10000) oddSquares
    in sum belowLimit
