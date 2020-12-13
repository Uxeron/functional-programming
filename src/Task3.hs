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


showGrid :: Grid -> IO ()
showGrid grid = do
    putStrLn [(getGridValue grid 0 0), ' ', (getGridValue grid 1 0), ' ', (getGridValue grid 2 0)]
    putStrLn [(getGridValue grid 0 1), ' ', (getGridValue grid 1 1), ' ', (getGridValue grid 2 1)]
    putStrLn [(getGridValue grid 0 2), ' ', (getGridValue grid 1 2), ' ', (getGridValue grid 2 2)]


--                - - - P A R S E   F U N C T I O N S - - -


parseDictLast :: String -> Grid -> Either (String, Int) (Grid, String)
parseDictLast ('l':'d':'4':':':'d':'a':'t':'a':'l':'i': x :'e':'i': y :'e':'1':':': v :'e':'e':'e' : t) grid = 
    case isXYValid (digitToInt x) (digitToInt y) of
        True -> case getGridValue grid (digitToInt x) (digitToInt y) == '_' of
            True -> Right ( setGridValue grid (digitToInt x) (digitToInt y) v, t)
            False -> Left ("Duplicate value", length t)
        False -> Left ("Invalid X/Y", length t)

parseDictLast ('4':':':'l':'a':'s':'t' : t) grid = parseDictLast t grid
parseDictLast t _ = Left ("Invalid dictionary Last", length t)


parseDictPrev :: String -> Grid -> Either (String, Int) (Grid, String)
parseDictPrev ('d' : t) grid = 
    case parseDict t grid of
        Right (prev, t') -> Right (prev, t')
        Left err -> Left err

parseDictPrev ('4':':':'p':'r':'e':'v' : t) grid = parseDictPrev t grid
parseDictPrev t _ = Left ("Invalid dictionary Previous", length t)


parseDict :: String -> Grid -> Either (String, Int) (Grid, String)
parseDict ('d' : t) grid = parseDict t grid
parseDict ('4':':':'l':'a':'s':'t' : t) grid = 
    case parseDictLast t grid of
        Right (grid', t') -> case t' of
            ('e' : t''') -> Right (grid', t''')
            _ -> case parseDictPrev t' grid' of
                Right (grid'', ('e' : t'')) -> Right (grid'', t'')
                Right _ -> Left ("Invalid dictionary", length t)
                Left err -> Left err
        Left err -> Left err

parseDict ('4':':':'p':'r':'e':'v' : t) grid = 
    case parseDictPrev t grid of
        Right (grid', t') -> case parseDictLast t' grid' of
            Right (grid'', ('e' : t'')) -> Right (grid'', t'')
            Right _ -> Left ("Invalid dictionary", length t)
            Left err -> Left err
        Left err -> Left err

parseDict ('e' : t) grid = Right (grid, t)
parseDict t _ = Left ("Invalid dictionary", length t)


parse :: String -> Either String Grid
parse msg = 
    case parseDict msg emptyGrid of
        Right (val, _) -> Right val
        Left (err, pos) -> case err == "Duplicate value" of
            True -> Left (err ++ " at position " ++ show (length msg - pos) ++ " 101")
            False -> Left (err ++ " at position " ++ show (length msg - pos) ++ " 100")
