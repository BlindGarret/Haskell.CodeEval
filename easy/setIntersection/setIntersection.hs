import System.Environment
import Data.List.Split
import Data.List

stringToInt :: String -> Int
stringToInt x = read x

formatLine :: String -> ([String], [String])
formatLine x = ((splitOn "," (splitSet !! 0)), (splitOn "," (splitSet !! 1)))
                where splitSet = splitOn ";" x

intersectSets :: ([String], [String]) -> String
intersectSets (set1, set2) = 
        (combine $ map (\x -> (x ++)) (sorted $ intersect set1 set2)) ""
            where   combine (x : xs) = foldl (\f s -> f . ("," ++) . s) x xs
                    combine [] = ("" ++)
                    sorted x = map show (sort $ map stringToInt x)

main :: IO ()
main = do
    args <- getArgs
    let path = args !! 0
    file <- readFile path
    putStrLn . unlines $ map (intersectSets . formatLine) (lines file)
