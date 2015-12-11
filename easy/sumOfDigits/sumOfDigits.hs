import System.Environment
import Data.Char

sumDigits :: [Int] -> Int
sumDigits [] = 0
sumDigits (x : xs) = x + (sumDigits xs)

toIntList :: String -> [Int]
toIntList [] = []
toIntList (x : xs) = (digitToInt x) : (toIntList xs)

main :: IO ()
main = do
    args <- getArgs
    let path = args !! 0
    file <- readFile path
    putStrLn . unlines $ map (show . sumDigits . toIntList) (lines file)