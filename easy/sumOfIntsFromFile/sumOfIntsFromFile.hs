import System.Environment

main :: IO ()
main = do
    args <- getArgs
    let path = args !! 0
    file <- readFile path
    let numbers = map read (lines file)
    putStrLn . show $ foldr (\x y -> x + y) (head numbers) (tail numbers)