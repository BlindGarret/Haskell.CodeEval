import System.Environment
import Data.Char

hexToDecimal :: String -> String
hexToDecimal x = show (sum (parse (reverse x) 0))
    where   parse [] _ = []
            parse (x : xs) i =  ((digitToInt x) * 16 ^ i) : parse xs (i + 1)

main :: IO ()
main = do
    args <- getArgs
    let path = args !! 0
    file <- readFile path
    putStrLn . unlines $ map hexToDecimal (lines file)