import Control.Monad
import Control.Monad.Reader

isPalindrome :: Int -> Bool
isPalindrome x = reverse (show x) == (show x)

isPrime :: Int -> Bool
isPrime x
    | x `mod` 2 == 0 = False
    | otherwise = (length (getFactors x)) == 0

getFactors :: Int -> [Int]
getFactors x = getFactorsInternal 3 (round . sqrt $ fromIntegral x) x

getFactorsInternal :: Int -> Int -> Int -> [Int]
getFactorsInternal x max original
    | x > max = []
    | original `mod` x == 0 = original : getFactorsInternal (x + 2) max original
    | otherwise = getFactorsInternal (x + 2) max original

main :: IO()
main = do
    putStrLn $ show ((filter (liftM2 (&&) isPrime isPalindrome) [1000, 999 .. 0]) !! 0)