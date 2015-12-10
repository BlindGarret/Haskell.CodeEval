import System.Environment
import Data.List.Split

stringToInt :: String -> Int
stringToInt x = read x :: Int

splitOnCommas :: String -> [String]
splitOnCommas = splitOn ","

format :: String -> [Int]
format x = map stringToInt (splitOnCommas x)

getMultiple :: [Int] -> Int
getMultiple [target, originalx, currentx] 
    | currentx >= target    = currentx
    | otherwise             = getMultiple [target, originalx, currentx + originalx]
getMultiple [target, originalx] 
    | originalx >= target   = originalx
    | otherwise             = getMultiple [target, originalx, originalx * 2] 

main :: IO ()
main = do
    args <- getArgs
    let path = args !! 0
    file <- readFile path
    putStrLn (unlines ( map (show . getMultiple . format) (lines file)))
