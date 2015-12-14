import Data.Char
import System.Environment

isArmstrongNumber :: Int -> Bool
isArmstrongNumber x = (sum set) == x
    where   set = map (\x -> x ^ setLength) (getDigits x)
            setLength = length (show x)

stringToInt :: String -> Int
stringToInt x = read x

getDigits :: Int -> [Int]
getDigits x = map digitToInt (show x)

main :: IO ()
main = do
    args <- getArgs
    let path = args !! 0
    file <- readFile path
    putStrLn . unlines $ map (show . isArmstrongNumber . stringToInt) (lines file)