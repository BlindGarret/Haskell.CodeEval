import System.Environment
import Data.List.Split

getUnique :: String -> String
getUnique x = init ((getUniqueInternal (splitOn "," x) " ") "")
                where   getUniqueInternal [] s = ("" ++)
                        getUniqueInternal (x:xs) s
                            | x == s    = getUniqueInternal xs s
                            | otherwise = (x ++) . ("," ++) . getUniqueInternal (xs) x 

main :: IO ()
main = do
    args <- getArgs
    let path = args !! 0
    file <- readFile path
    putStrLn . unlines $ map getUnique (lines file)