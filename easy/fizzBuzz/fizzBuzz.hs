import System.Environment

fizzBuzzSingle :: Int -> Int -> Int -> String
fizzBuzzSingle f b n
    | n `mod` f == 0 && n `mod` b == 0  = "FB"
    | n `mod` f == 0                    = "F"
    | n `mod` b == 0                    = "B"
    | otherwise                         = show n

fizzBuzz :: (Int, Int, Int) -> [String]
fizzBuzz (f,b,end) = map (fizzBuzzSingle f b) [1..end]

convertInputLine :: String -> (Int, Int, Int)
convertInputLine x = packageInputs ((map read . words) x :: [Int])

packageInputs :: [Int] -> (Int, Int, Int)
packageInputs [f,b,end] = (f, b, end)

handleFizzBuzz :: [String] -> [String]
handleFizzBuzz [] = []
handleFizzBuzz (x : xs) =
    (unwords . fizzBuzz . convertInputLine $ x) : handleFizzBuzz xs

main :: IO ()
main = do
    args <- getArgs
    let path = args !! 0
    file <- readFile path
    putStrLn . unlines . handleFizzBuzz . lines $ file
