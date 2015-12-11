import System.Environment

reverseWords :: String -> String
reverseWords x = unwords . reverse . words $ x

main :: IO ()
main = do
    args <- getArgs
    let path = args !! 0
    file <- readFile path
    putStrLn . unlines $ map reverseWords (lines file)
