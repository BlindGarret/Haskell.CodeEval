import System.Environment
import Data.List.Split

myMod :: Int -> Int -> Int
myMod n m = n - (n `quot` m * m)

handleLine :: String -> String
handleLine line = show ((intSet !! 0) `myMod` (intSet !! 1))
    where   splitLine = splitOn "," line
            intSet = map stringToInt splitLine

stringToInt :: String -> Int
stringToInt x = read x

main :: IO ()
main = do
    args <- getArgs
    let path = args !! 0
    file <- readFile path
    putStrLn . unlines $ map handleLine (lines file)