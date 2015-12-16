import Data.List
import Data.Char
import Data.Ord
import System.Environment

countSort :: Ord a => [a] -> [[a]]
countSort x =  sortBy (flip $ comparing length) (group $ sort x)

maximumStringBeauty :: String -> Int
maximumStringBeauty x = maximumBeautyInner (countSort . map toUpper $ filter isAlpha x) 26
    where maximumBeautyInner [] _ = 0
          maximumBeautyInner (x : xs) maxVal = (length x) * maxVal + maximumBeautyInner xs (maxVal - 1)

main :: IO ()
main = do
    args <- getArgs
    let path = args !! 0
    file <- readFile path
    putStrLn . unlines . map (show . maximumStringBeauty) . lines $ fil