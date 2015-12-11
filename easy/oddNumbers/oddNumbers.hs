import System.Environment

main :: IO ()
main = do
    putStrLn . unlines . map show $ filter (\x -> x `mod` 2 /= 0) [1..100]