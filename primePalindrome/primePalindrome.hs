import Control.Monad
import Control.Monad.Reader

isPalindrome :: Int -> Bool
isPalindrome x = reverse (show x) == (show x)

isPrime :: Int -> Bool
isPrime x
    | x `mod` 2 == 0 = False
    | null (filter (\y -> x `mod` y == 0) [3,5..(round . sqrt $ fromIntegral x)]) = True
    | otherwise = False

main :: IO()
main = do
    putStrLn $ show ((filter (liftM2 (&&) isPrime isPalindrome) [1000, 999 .. 0]) !! 0)