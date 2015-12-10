getMyLine :: Int -> String
getMyLine x = (show x) ++ ",1,1,1,1,1,1,1"

main :: IO()
main = do
    appendFile "sku.csv" . unlines $ map getMyLine [1,2..500000] 