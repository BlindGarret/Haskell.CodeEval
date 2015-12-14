import System.Environment
import Data.List.Split

handleLine :: String -> String
handleLine x = rightMostIndex (split !! 0) ((split !! 1) !! 0)
                where split = splitOn "," x

rightMostIndex :: String -> Char -> String
rightMostIndex [] c = ""
rightMostIndex s c = show (recurse s c 0 (-1))
                    where recurse (s : xs) c counter index
                            | s == c    = recurse xs c (counter + 1) counter
                            | otherwise = recurse xs c (counter + 1) index
                          recurse [] c counter index = index

main :: IO ()
main = do
    args <- getArgs
    let path = args !! 0
    file <- readFile path
    putStrLn . unlines $ filter (\x -> x /= []) (map handleLine (lines file))