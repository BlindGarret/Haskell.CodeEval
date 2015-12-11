isPrime :: Int -> Bool
isPrime x
    | x == 1 = False
    | x == 2 = True
    | x `mod` 2 == 0 = False
    | null (filter (\y -> x `mod` y == 0) [3,5 .. (round .sqrt $ fromIntegral x)]) = True
    | otherwise = False

sumOfPrimes :: Int -> Int
sumOfPrimes x = sum (take x (filter isPrime [2,3..]))

main :: IO()
main = do
    putStrLn . show $ sumOfPrimes 1000