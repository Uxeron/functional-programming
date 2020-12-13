module Task3 where

import Data.Char

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

setGridValue :: Grid -> Int -> Int -> Char -> Grid
setGridValue grid 0 y v = (setRowValue (t0 grid) y v, t1 grid, t2 grid)
setGridValue grid 1 y v = (t0 grid, setRowValue (t1 grid) y v, t2 grid)
setGridValue grid 2 y v = (t0 grid, t1 grid, setRowValue (t2 grid) y v)

setRowValue :: Row -> Int -> Char -> Row
setRowValue row 0 v = (v, t1 row, t2 row)
setRowValue row 1 v = (t0 row, v, t2 row)
setRowValue row 2 v = (t0 row, t1 row, v)


getGridValue :: Grid -> Int -> Int -> Char
getGridValue grid 0 y = getRowValue (t0 grid) y
getGridValue grid 1 y = getRowValue (t1 grid) y
getGridValue grid 2 y = getRowValue (t2 grid) y

getRowValue :: Row -> Int -> Char
getRowValue row 0 = t0 row
getRowValue row 1 = t1 row
getRowValue row 2 = t2 row


isGridFull :: Grid -> Bool
isGridFull grid = isRowFull (t0 grid) && isRowFull (t1 grid) && isRowFull (t2 grid)

isRowFull :: Row -> Bool
isRowFull row = (t0 row /= '_') && (t1 row /= '_') && (t2 row /= '_')


isXYValid :: Int -> Int -> Bool
isXYValid x y = (x >= 0) && (x < 3) && (y >= 0) && (y < 3)
