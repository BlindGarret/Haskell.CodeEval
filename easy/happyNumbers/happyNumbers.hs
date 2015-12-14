import Data.List
import Data.Char
import System.Environment

isNumberHappy :: Int -> Int
isNumberHappy x 
    | (numberHappyInternal (handleDigits x) []) == 1    = 1
    | otherwise                                         = 0
        where numberHappyInternal x set
                | (intersect set [handleDigits x]) /= [] = handleDigits x
                | handleDigits x == 1 = 1
                | otherwise = numberHappyInternal (handleDigits x) (x : set)
              handleDigits x = foldr (\x y -> x + y) (head (getSquareDigits)) (tail (getSquareDigits))
                where getSquareDigits = map ((\num -> num * num) . digitToInt ) (show x)

stringToInt :: String -> Int
stringToInt x = read x

main :: IO ()
main = do
    args <- getArgs
    let path = args !! 0
    file <- readFile path
    putStrLn . unlines $ map (show . isNumberHappy . stringToInt)  (lines file)