module Task4 where

import Data.List
import Data.Char
import System.Exit
import System.IO
import Control.Monad

data JsonLikeValue = JLString String | JLInt Int | JLMap [(String, JsonLikeValue)] | JLArray [JsonLikeValue] deriving (Show, Eq)

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

