data QueryBoard = QueryBoard [[Int]]

empty :: QueryBoard
empty = QueryBoard $ cycle [cycle [0]]

setCol :: QueryBoard -> Int -> Int ->  QueryBoard
setCol (QueryBoard rows) colNum val = QueryBoard $ map (zipWith ($) (buildColFilter colNum val)) rows

queryCol :: QueryBoard -> Int -> [Int]
queryCol (QueryBoard rows) colNum = map (\x -> x !! colNum) rows

setRow :: QueryBoard -> Int -> Int -> QueryBoard
setRow (QueryBoard rows) rowNum val = QueryBoard $ zipWith (zipWith ($)) (buildRowFilter rowNum val) rows

queryRow :: QueryBoard -> Int -> [Int]
queryRow (QueryBoard rows) rowNum = rows !! rowNum

buildColFilter :: Int -> Int -> [Int -> Int]
buildColFilter x y
    | x < 0     = id : buildColFilter (-1) y
    | x > 0     = id : buildColFilter (x - 1) y
    | otherwise = (\i -> y) : buildColFilter (x - 1) y

buildRowFilter :: Int -> Int -> [[Int -> Int]]
buildRowFilter x y
    | x < 0     = cycle [id] : buildRowFilter (-1) y
    | x > 0     = cycle [id] : buildRowFilter (x - 1) y
    | otherwise = cycle [(\i -> y)] : buildRowFilter (x - 1) y