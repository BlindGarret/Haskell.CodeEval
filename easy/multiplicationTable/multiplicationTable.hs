import Data.Text (stripEnd, pack, unpack, Text)
import Data.List

formatTextWithSpaces :: String -> Int -> String
formatTextWithSpaces input padTo =  (take (padTo - (length input)) spaces) ++ input
                                    where spaces = ' ' : spaces

buildLine :: Int -> Int -> Int -> String
buildLine counter max start= unpack . stripEnd $ pack ((buildLineInner counter max start) "")
    where buildLineInner counter max start
            | counter > max   = ("" ++)
            | otherwise       =  ((formatTextWithSpaces (show (start * counter)) 4) ++) .
                                    (buildLineInner (counter + 1) max start)

buildTable :: Int -> [String]
buildTable max = map (buildLine 1 max) [1 .. max]

main :: IO()
main = do
    putStrLn . unlines $ buildTable 12