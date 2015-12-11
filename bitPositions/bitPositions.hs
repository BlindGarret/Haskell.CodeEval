import System.Environment
import Data.List.Split

toBinary :: Int -> [Int]
toBinary 0 = [0]
toBinary x = toBinary(x `quot` 2) ++ [x `rem` 2]

bitPositions :: Int -> Int -> Int -> Bool
bitPositions val fst scd = x !! (fst - 1) == x !! (scd - 1) 
                            where x = reverse (toBinary val)

formattedBitPositions :: Int -> Int -> Int -> String
formattedBitPositions val fst scd
    | bitPositions val fst scd == True = "true"
    | bitPositions val fst scd == False = "false"

handleBitPositions :: String -> String
handleBitPositions x = formattedBitPositions (ints !! 0) (ints !! 1) (ints !! 2)
                            where ints = map read (splitOn "," x)

main :: IO ()
main = do
    args <- getArgs
    let path = args !! 0
    file <- readFile path
    putStrLn . unlines $ map handleBitPositions (lines file)
