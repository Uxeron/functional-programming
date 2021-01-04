module Task4 where

import Data.List
import Data.Char
import Data.Ord
import System.Exit
import System.IO
import Control.Monad

data JsonLikeValue = JLString String | JLInt Int | JLMap [(String, JsonLikeValue)] | JLArray [JsonLikeValue] deriving (Show, Eq)
type StringPath = (String, String)

--                - - - P A R S E   F U N C T I O N S - - -

-- Parses a single integer
-- IN: Bencoded string that looks like i...e (i0e)
-- OUT: Left: Error message, how much of the bencoded string was left to parse
--     Right: Parsed JLInt, rest of the bencoded string
parseInt :: String -> Either (String, Int) (JsonLikeValue, String)
parseInt ('i' : t) = case rest of
        ('e' : r) | (length number) > 0 -> Right (JLInt (read number), r)
        otherwise -> Left ("Invalid integer", length t)
    where
        number = takeWhile isDigit t
        rest = drop (length number) t


-- Parses a single string
-- IN: Bencoded string that looks like length:string (2:ad)
-- OUT: Left: Error message, how much of the bencoded string was left to parse
--     Right: Parsed JLString, rest of the bencoded string
parseString :: String -> Either (String, Int) (JsonLikeValue, String)
parseString str = case rest of 
        (':' : r) | (length size) > 0 && (length r >= (read size)) 
            -> Right (JLString (take (read size) r), drop (read size) r)
        otherwise -> Left ("Invalid string", length str)
    where
        size = takeWhile isDigit str
        rest = drop (length size) str


-- Parses a list
-- IN: Bencoded string that looks like l...e
-- OUT: Left: Error message, how much of the bencoded string was left to parse
--     Right: Parsed JLArray, rest of the bencoded string
parseList :: String -> Either (String, Int) (JsonLikeValue, String)
parseList ('e' : t) = Right (JLArray [], t)
parseList str = case parseValue str of -- Parse the value
        Right (val, t') -> case parseList t' of -- Parse the next list entry
            Right (JLArray list, t'') -> Right (JLArray (val : list), t'')
            Left err -> Left err
        Left err -> Left err


-- Parses a dictionary
-- IN: Bencoded string that looks like d...e
-- OUT: Left: Error message, how much of the bencoded string was left to parse
--     Right: Parsed JLMap, rest of the bencoded string
parseDict :: String -> Either (String, Int) (JsonLikeValue, String)
parseDict ('e' : t) = Right (JLMap [], t)
parseDict str = case parseString str of -- Parse the key
        Right (JLString key, t) -> case parseValue t of -- Parse the value
            Right (val, t') -> case parseDict t' of -- Parse the next dictionary entry
                Right (JLMap dict, t'') -> Right (JLMap ([(key, val)] ++ dict), t'')
                Left err -> Left err
            Left err -> Left err
        Left err -> Left err


-- Parses any bencoded data structure 
-- IN: Bencoded string
-- OUT: Left: Error message, how much of the bencoded string was left to parse
--     Right: Parsed JLMap, rest of the bencoded string
parseValue :: String -> Either (String, Int) (JsonLikeValue, String)
parseValue ('e' : t) = Right (JLArray [], t)  -- End of data structure, return empty array
parseValue ('i' : t) = parseInt ("i" ++ t)    -- Parse an Integer
parseValue ('l' : t) = parseList t            -- Parse a List
parseValue ('d' : t) = parseDict t            -- Parse a dictionary
parseValue (a : b : c : t) = parseString ([a, b, c] ++ t) -- Parse a string (needs to be at least 0:e, 3 symbols)
parseValue _ = Left ("Invalid symbol", 0)     -- Invalid entry


-- Calls the actual bencode parser and handles errors
-- IN: Bencoded string
-- OUT: Parsed data, error found, error message
parse :: String -> (JsonLikeValue, Bool, String)
parse str = case parseValue str of
    Right (val, _) -> (val, False, "")
    Left (err, pos) -> (JLArray [], True, err ++ " at position " ++ show (length str - pos))


--                - - - D E P T H   -   F I R S T   S E A R C H - - -

-- Does a depth-first search of the given JsonLikeValues and finds any strings
-- IN: Path, data stored as JsonLikeValue
-- OUT: List of (path, string) entries
dfs :: String -> JsonLikeValue -> [StringPath]
dfs path (JLArray list) = concat (map (dfsAppendPathList path) (zip [0..] list))
dfs path (JLMap dict) = concat (map (dfsAppendPathDict path) dict)
dfs path (JLInt int) = []
dfs path (JLString str) = [(path ++ " = ", str)]


-- Appends the appropriate path for a list entry
-- IN: Path, list index, data
-- OUT: List of (path, string) entries
dfsAppendPathList :: String -> (Int, JsonLikeValue) -> [StringPath]
dfsAppendPathList path (index, val) = dfs (path ++ "[" ++ (show index) ++ "]") val


-- Appends the appropriate path for a dictionary entry
-- IN: Path, dictionary key, data
-- OUT: List of (path, string) entries
dfsAppendPathDict :: String -> (String, JsonLikeValue) -> [StringPath]
dfsAppendPathDict path (key, val) = dfs (path ++ "." ++ key) val


--                        - - - M A I N   C O D E - - -

-- Prints the first five strings in the passed (path, string) list
-- IN: List of (path, string) entries, list index
-- OUT: IO
printFirst5Strings :: [StringPath] -> Int -> IO ()
printFirst5Strings list index
    | index > 4 = return ()
    | index < length list = do
        putStrLn (fst (list!!index) ++ "\"" ++  snd (list!!index) ++ "\"")
        printFirst5Strings list (index + 1)
    | otherwise = return ()


-- Prints message and then exits with exit code
-- IN: Error found, error message
-- OUT: IO
exitWithMessage :: Bool -> String -> IO ()
exitWithMessage error message = do
    if error
        then do
            hPutStrLn stderr message
            exitWith (ExitFailure 1) 
        else exitSuccess


-- Main function
main :: IO ()
main = do
    message <- getLine
    let (parsedData, error, errMsg) = parse message
    when error (exitWithMessage True errMsg)

    let stringPathList = dfs "root" parsedData
    let stringPathListSorted = sortBy (comparing (length . snd)) stringPathList

    printFirst5Strings stringPathListSorted 0

    exitWithMessage False ""
