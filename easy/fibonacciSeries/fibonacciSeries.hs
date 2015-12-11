import System.Environment

fibSet :: [Integer]
fibSet = 0 : 1 : getFibs 0 1
    where getFibs x y = z : getFibs y z
            where z = x + y

getFib :: Int -> Integer
getFib x = fibSet !! x

main :: IO ()
main = do
    args <- getArgs
    let path = args !! 0
    file <- readFile path
    putStrLn . unlines $ map (show . getFib . read) (lines file)