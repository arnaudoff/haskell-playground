data Shape = Circle Float | Rectangle Float Float deriving (Eq, Ord, Show, Read)

circ = Circle 12.52
rect = Rectangle 13.53 42.92

data RGB = Red | Green | Blue
data Week = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
    deriving (Eq, Ord, Enum, Read)

instance Show Week where
    show Monday = "Mon"
    show Tuesday = "Tue"
    show Wednesday = "Wed"
    show Thursday = "Thu"
    show Friday = "Fri"
    show Saturday = "Sat"
    show Sunday = "Sun"

isRound :: Shape -> Bool
isRound (Circle _) = True
isRound (Rectangle _ _) = False

area :: Shape -> Maybe Float
area (Circle r)
    | r < 0 = Nothing
    | otherwise = Just (pi * r * r)
area (Rectangle h w)
    | h < 0 || w < 0 = Nothing
    | otherwise = Just (h * w)

data Expr =
    Lit Int | Add Expr Expr | Sub Expr Expr | Mult Expr Expr | Div Expr Expr

expr_val :: Expr -> Int
expr_val (Lit a) = a
expr_val (Add e1 e2) = expr_val e1 + expr_val e2
expr_val (Sub e1 e2) = expr_val e1 - expr_val e2
expr_val (Mult e1 e2) = expr_val e1 * expr_val e2
expr_val (Div e1 e2) = expr_val e1 `div` expr_val e2
