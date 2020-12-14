module Main where

import Data.Char
import System.Exit
import System.IO
import Control.Monad

--                - - - D A T A - - -

type Row = (Char, Char, Char)
type Grid = (Row, Row, Row)

emptyGrid = (('_', '_', '_'), ('_', '_', '_'), ('_', '_', '_'))
defaultGrid = (('_', '_', '_'), ('_', 'X', '_'), ('_', '_', '_'))


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

--                - - - G R I D   &   R O W   F U N C T I O N S - - -

t0 :: (v, v, v) -> v
t0 (a, _, _) = a

t1 :: (v, v, v) -> v
t1 (_, b, _) = b

t2 :: (v, v, v) -> v
t2 (_, _, c) = c


setGridValue :: Grid -> Char -> Int -> Int -> Grid
setGridValue grid v 0 y = (setRowValue (t0 grid) v y, t1 grid, t2 grid)
setGridValue grid v 1 y = (t0 grid, setRowValue (t1 grid) v y, t2 grid)
setGridValue grid v 2 y = (t0 grid, t1 grid, setRowValue (t2 grid) v y)

setRowValue :: Row -> Char -> Int -> Row
setRowValue row v 0 = (v, t1 row, t2 row)
setRowValue row v 1 = (t0 row, v, t2 row)
setRowValue row v 2 = (t0 row, t1 row, v)


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


hasWonHorizontal :: Grid -> Char -> Int -> Bool
hasWonHorizontal grid v y = (getGridValue grid 0 y == v) && (getGridValue grid 1 y == v) && (getGridValue grid 2 y == v)

hasWonVertical   :: Grid -> Char -> Int -> Bool
hasWonVertical   grid v x = (getGridValue grid x 0 == v) && (getGridValue grid x 1 == v) && (getGridValue grid x 2 == v)

hasWonDiagonal   :: Grid -> Char -> Bool
hasWonDiagonal   grid v   = (getGridValue grid 0 0 == v) && (getGridValue grid 1 1 == v) && (getGridValue grid 2 2 == v)

hasWonDiagonal'  :: Grid -> Char -> Bool
hasWonDiagonal'  grid v   = (getGridValue grid 2 0 == v) && (getGridValue grid 1 1 == v) && (getGridValue grid 0 2 == v)

hasWon :: Grid -> Char -> Bool
hasWon g v = (hasWonHorizontal g v 0) || (hasWonHorizontal g v 1) || (hasWonHorizontal g v 2) || 
             (hasWonVertical   g v 0) || (hasWonVertical   g v 1) || (hasWonVertical   g v 2) ||
             (hasWonDiagonal   g v)   || (hasWonDiagonal'  g v)

hasWonAny :: Grid -> Bool
hasWonAny grid = (hasWon grid 'X') || (hasWon grid 'O')


isXYValid :: Int -> Int -> Bool
isXYValid x y = (x >= 0) && (x < 3) && (y >= 0) && (y < 3)


invertSymbol :: Char -> Char
invertSymbol 'X' = 'O'
invertSymbol 'O' = 'X'


showGrid :: Grid -> IO ()
showGrid grid = do
    hPutStrLn stderr [(getGridValue grid 0 0), ' ', (getGridValue grid 1 0), ' ', (getGridValue grid 2 0)]
    hPutStrLn stderr [(getGridValue grid 0 1), ' ', (getGridValue grid 1 1), ' ', (getGridValue grid 2 1)]
    hPutStrLn stderr [(getGridValue grid 0 2), ' ', (getGridValue grid 1 2), ' ', (getGridValue grid 2 2)]


--                - - - P A R S E   F U N C T I O N S - - -


-- Parses the "Last" dictionary branch, extracting the x y and symbol and putting them into the grid
-- IN: Bencoded string, grid to fill
-- OUT: Left: Error message, how much of the bencoded string was left to parse
--      Right: Filled grid, last move's symbol, remaining bencoded string
parseDictLast :: String -> Grid -> Either (String, Int) (Grid, Char, String)
parseDictLast ('l':'d':'4':':':'d':'a':'t':'a':'l':'i': x :'e':'i': y :'e':'1':':': v :'e':'e':'e' : t) grid = 
    if isXYValid (digitToInt x) (digitToInt y) then
        if getGridValue grid (digitToInt x) (digitToInt y) == '_'
        then Right ( setGridValue grid v (digitToInt x) (digitToInt y), v, t)
        else Left ("Duplicate value", length t)
    else Left ("Invalid X/Y", length t)

parseDictLast ('4':':':'l':'a':'s':'t' : t) grid = parseDictLast t grid
parseDictLast t _ = Left ("Invalid dictionary Last", length t)


-- Parses the "Prev" dictionary branch by calling the branching parseDict function
-- IN: Bencoded string, grid to fill
-- OUT: Left: Error message, how much of the bencoded string was left to parse
--      Right: Filled grid, remaining bencoded string
-- Note: The correct Last Move will never come through parseDictPrev function, it will come from parseDict's direct call to parseDictLast, safe to just ignore it here
parseDictPrev :: String -> Grid -> Either (String, Int) (Grid, String)
parseDictPrev ('d' : t) grid = 
    case parseDict t grid of
        Right (grid', _, t') -> Right (grid', t')
        Left err -> Left err

parseDictPrev ('4':':':'p':'r':'e':'v' : t) grid = parseDictPrev t grid
parseDictPrev t _ = Left ("Invalid dictionary Previous", length t)


-- Parses both "Prev" and "Last" branches of the general dictionary
-- IN: Bencoded string, grid to fill
-- OUT: Left: Error message, how much of the bencoded string was left to parse
--      Right: Filled grid, last move's symbol, remaining bencoded string
parseDict :: String -> Grid -> Either (String, Int) (Grid, Char, String)
parseDict ('d' : t) grid = parseDict t grid
parseDict ('4':':':'l':'a':'s':'t' : t) grid = 
    case parseDictLast t grid of
        Right (grid', lastMove, t') -> case t' of
            ('e' : t''') -> Right (grid', lastMove, t''') -- Dict was (Prev, Last), Last parsed, nowhere further to go on this branch
            _ -> case parseDictPrev t' grid' of           -- Dict is (Last, Prev), Last parsed, continue on to Prev
                Right (grid'', ('e' : t'')) -> Right (grid'', lastMove, t'')
                Right _ -> Left ("Invalid dictionary", length t)
                Left err -> Left err
        Left err -> Left err

parseDict ('4':':':'p':'r':'e':'v' : t) grid = 
    case parseDictPrev t grid of
        Right (grid', t') -> case parseDictLast t' grid' of -- The branching dicts will always eventually end on a "Last" branch, so no need to double check before parsing
            Right (grid'', lastMove, ('e' : t'')) -> Right (grid'', lastMove, t'')
            Right _ -> Left ("Invalid dictionary", length t)
            Left err -> Left err
        Left err -> Left err

parseDict ('e' : t) grid = Right (grid, '_', t)
parseDict t _ = Left ("Invalid dictionary", length t)


-- Start function for parsing, calls the real parsers and appends the appropriate exit code
-- IN: Input bencoded string
-- OUT: Filled grid, last move's symbol, exit code
parse :: String -> (Grid, Char, Int)
parse msg 
    | msg == "*" = (emptyGrid, 'O', 0)
    | otherwise = case parseDict msg emptyGrid of
        Right (val, lastMove, _) -> (val, lastMove, 0)
        Left (err, _) -> if err == "Duplicate value" 
            then (emptyGrid, '_', 101)
            else (emptyGrid, '_', 100)


--                - - - E N C O D E   F U N C T I O N S - - -


-- Appends a bencoded representation of a new move to the existing move chain
-- IN: Input bencoded string (or *), move x, move y, move symbol
-- OUT: Bencoded string with new move appended
encode :: String -> Int -> Int -> Char -> String
encode msg x y v = "d4:lastld4:datali" ++ show x ++ "ei" ++ show y ++ "e1:" ++ [v] ++ "eee" ++ msg' ++ "e"
    where
        msg' = if msg == "*" then ""
            else "4:prev" ++ msg


--                - - - M I N I M A X   A L G O R I T H M   F U N C T I O N S - - -


-- Checks if anyone has won the game and returns the appropriate score
-- IN: Game grid, current player's symbol
-- OUT:  10 if current player has won
--      -10 if current player's opponent has won
--        0 if noone has won    
evaluateGridScore :: Grid -> Char -> Int
evaluateGridScore grid v 
    | hasWon grid v = 10
    | hasWon grid (invertSymbol v) = -10
    | otherwise = 0

-- Advances the passed X Y coordinates by 1, simple helper function
-- IN: X, Y to advance
-- OUT: X, Y advanced by one, X kept below 3
advanceXY :: Int -> Int -> (Int, Int)
advanceXY x y
    | (x + 1) > 2 = (0, y + 1)
    | otherwise = (x + 1, y)


-- Checks if any additional moves can be made, and advances the minimax algorithm if additional moves are possible
-- IN: Game grid, move depth, real player, currently checked player
-- OUT: Best score
minimax :: Grid -> Int -> Char -> Char -> Int
minimax grid depth rv v 
    | score ==  10 = score - depth
    | score == -10 = score + depth
    | isGridFull grid = 0
    | otherwise = callMinimaxOnGrid grid (depth + 1) rv (invertSymbol v)
    where
        score = evaluateGridScore grid rv


-- Attempts to call minimax on the given cell. If cell is not already filled in, will fill it and call minimax.
-- IN: Game grid, move depth, real player, currently checked player, cell x, cell y
-- OUT: Best score
tryCallMinimax :: Grid -> Int -> Char -> Char -> Int -> Int -> Int
tryCallMinimax grid depth rv v x y
    | getGridValue grid x y == '_' =
        minimax (setGridValue grid v x y) depth rv v
    | otherwise = 
        if rv == v then -1000 -- Trying to maximize
        else 1000 -- Trying to minimize


-- Attempts to call minimax on the entire grid and gets the maximum/minimum score of all minimax calls
-- IN: Game grid, move depth, real player, currently checked player 
-- OUT: Best score
callMinimaxOnGrid :: Grid -> Int -> Char -> Char -> Int
callMinimaxOnGrid grid depth rv v
    | rv == v = maximum values -- Trying to maximize
    | otherwise = minimum values -- Trying to minimize
    where 
        values = [(tryCallMinimax grid depth rv v 0 0), (tryCallMinimax grid depth rv v 1 0), (tryCallMinimax grid depth rv v 2 0),
                  (tryCallMinimax grid depth rv v 0 1), (tryCallMinimax grid depth rv v 1 1), (tryCallMinimax grid depth rv v 2 1),
                  (tryCallMinimax grid depth rv v 0 2), (tryCallMinimax grid depth rv v 1 2), (tryCallMinimax grid depth rv v 2 2)]
        

-- Finds the move with the highest score for the given grid and player
-- IN: Game grid, player, cell x, cell y
-- OUT: Best move x, best move y, best score
findBestMove' :: Grid -> Char -> Int -> Int -> (Int, Int, Int)
findBestMove' grid v x y 
    | score > score' = (x, y, score)
    | otherwise = (x'', y'', score')
    where
        score = tryCallMinimax grid 0 v v x y

        (x', y') = advanceXY x y
        (x'', y'', score') = if y' < 3
            then findBestMove' grid v x' y'
            else (0, 0, -1000) -- Reached the end of the grid


-- Calls the findBestMove function and appends the appropriate exit code
-- IN: Game grid, player
-- OUT: Best move x, best move y, exit code
findBestMove :: Grid -> Char -> (Int, Int, Int)
findBestMove grid v = (x, y, code)
    where
        (x, y, _) = findBestMove' grid v 0 0
        grid' = setGridValue grid v x y
        code
            | hasWonAny grid' = 
                if hasWon grid' v 
                    then 10 -- Victory
                    else - 1000 -- Loss - We should _not_ be here
            | isGridFull grid' = 12
            | otherwise = 0


-- Checks if the game can continue to be played before calling findBestMove
-- IN: Game grid, player
-- OUT: Best move x, best move y, exit code
findBestMovePregameCheck :: Grid -> Char -> (Int, Int, Int)
findBestMovePregameCheck grid v 
    | isGridFull grid = (0, 0, 20)
    | hasWonAny grid = (0, 0, 20)
    | grid == emptyGrid = (1, 1, 0) -- Default case, the first optimal move is center
    | otherwise = findBestMove grid v


--                - - - M A I N   C O D E - - -


-- Maps exit codes to output messages
-- IN: Exit code
-- OUT: Informational message
exitCodeMessage :: Int -> String
exitCodeMessage 0   = "We are in the middle of a game"
exitCodeMessage 10  = "Game over, I won!"
exitCodeMessage 12  = "Game over, Draw!"
exitCodeMessage 20  = "Game is over, board is full or someone won"
exitCodeMessage 100 = "Message is malformed"
exitCodeMessage 101 = "Message is invalid, 2 moves to the same cell"
exitCodeMessage _   = "Unknown exit code"


-- Prints message and then exits with exit code
-- IN: Exit code
-- OUT: 
exitWithMessage :: Int -> IO ()
exitWithMessage code = do
    hPutStrLn stderr (exitCodeMessage code)
    if code == 0 
        then exitSuccess
        else exitWith (ExitFailure code)


-- Main function
main :: IO ()
main = do
    message <- getLine
    let (grid, v, code) = parse message
    when (code /= 0) (exitWithMessage code) -- Bencoded message was invalid
    
    hPutStrLn stderr "Current game state:"
    showGrid grid
    
    let v' = invertSymbol v
    let (x, y, code') = findBestMovePregameCheck grid v'
    when (code' == 20) (exitWithMessage code') -- Game has already ended
    
    hPutStrLn stderr "My move: "
    hPutStrLn stderr (show x ++ " " ++ show y ++ " " ++ [v'])

    hPutStrLn stderr "State after my move:"
    showGrid (setGridValue grid v' x y)

    let message' = encode message x y v'
    putStrLn message'
    
    exitWithMessage code'

