import Data.Char
import System.Environment

isNumberSelfDescribing :: Int -> Int
isNumberSelfDescribing x
    | (testDigits (map digitToInt (show x)) 0) == True  = 1
    | otherwise                               = 0
    where testDigits x i
            | i == length x                                  = True  
            | (length $ filter (\y -> y == i) x) /= x !! i   = False
            | otherwise                                      = testDigits x (i + 1)

stringToInt :: String -> Int
stringToInt x = read x

main :: IO ()
main = do
    args <- getArgs
    let path = args !! 0
    file <- readFile path
    putStrLn . unlines $ map (show . isNumberSelfDescribing . stringToInt)  (lines file)