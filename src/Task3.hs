module Task3 where

type Row = (Char, Char, Char)
type Grid = (Row, Row, Row)

emptyGrid = (('_', '_', '_'), ('_', '_', '_'), ('_', '_', '_'))

-- Grid:
-- ((a, b, c), (d, e, f), (g, h, i))
--
--    0 1 2 x
--  0 a d g
--  1 b e h
--  2 c f i
--  y
--
-- Valid values: X, O, _

t0 :: (v, v, v) -> v
t0 (a, _, _) = a

t1 :: (v, v, v) -> v
t1 (_, b, _) = b

t2 :: (v, v, v) -> v
t2 (_, _, c) = c

--                - - - G R I D   &   R O W   F U N C T I O N S - - -

setGridValue :: Grid -> Int -> Int -> Char -> Either String Grid
setGridValue grid 0 y v = 
    case setRowValue (t0 grid) y v of
        Right row -> Right (row, t1 grid, t2 grid)
        Left err -> Left err
setGridValue grid 1 y v = 
    case setRowValue (t1 grid) y v of
        Right row -> Right (t0 grid, row, t2 grid)
        Left err -> Left err
setGridValue grid 2 y v = 
    case setRowValue (t2 grid) y v of
        Right row -> Right (t0 grid, t1 grid, row)
        Left err -> Left err
setGridValue _ _ _ _ = Left "Invalid x index"

setRowValue :: Row -> Int -> Char -> Either String Row
setRowValue row 0 v = Right (v, t1 row, t2 row)
setRowValue row 1 v = Right (t0 row, v, t2 row)
setRowValue row 2 v = Right (t0 row, t1 row, v)
setRowValue _ _ _ = Left "Invalid y index"


getGridValue :: Grid -> Int -> Int -> Either String Char
getGridValue grid 0 y = getRowValue (t0 grid) y
getGridValue grid 1 y = getRowValue (t1 grid) y
getGridValue grid 2 y = getRowValue (t2 grid) y
getGridValue _ _ _ = Left "Invalid x index"

getRowValue :: Row -> Int -> Either String Char
getRowValue row 0 = Right (t0 row)
getRowValue row 1 = Right (t1 row)
getRowValue row 2 = Right (t2 row)
getRowValue _ _ = Left "Invalid y index"


isGridFull :: Grid -> Bool
isGridFull grid = isRowFull (t0 grid) && isRowFull (t1 grid) && isRowFull (t2 grid)

isRowFull :: Row -> Bool
isRowFull row = (t0 row /= '_') && (t1 row /= '_') && (t2 row /= '_')