import Data.Char
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    let path = args !! 0
    file <- readFile path
    putStrLn . unlines $ map (map toLower) (lines file)