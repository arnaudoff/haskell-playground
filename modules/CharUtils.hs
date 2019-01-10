module CharUtils where
    import Data.Char

    ord' :: Char -> Int
    ord' = fromEnum

    chr' :: Int -> Char
    chr' = toEnum

    isDigit' :: Char -> Bool
    isDigit' x = x >= '0' && x <= '9'

    toUpper' :: Char -> Char
    toUpper' x = chr (ord x + ord 'A' - ord 'a')

    digits' :: String -> String
    digits' str = [ch | ch <- str, isDigit ch]
