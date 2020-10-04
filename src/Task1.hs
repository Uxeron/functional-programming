module Task1 where

import Task1Message

import Data.Char

parseDict :: String -> ((Int, Char), String)
parseDict ('1':':':'v':'1':':' : sym : '1':':':'x':'1':':' : x : 'e' : t) = ((digitToInt x, sym), t)
parseDict _ = error "Invalid dictionary"


parseList :: String -> ([(Int, Char)], String)
parseList ('d' : t) = 
      (val : list, str1)
      where
            (val, str) = parseDict t
            (list, str1) = parseList str

parseList ('e' : t) = ([], t)
parseList _ = error "Invalid list"


parseLL :: String -> ([[(Int, Char)]], String)
parseLL ('l' : t) = 
      (list : ll, str1)
      where 
            (list, str) = parseList t
            (ll, str1) = parseLL str

parseLL ('e' : t) = ([], t)
parseLL _ = error "Invalid list"


parse :: Int    -- ^ Size of the matrix (number of columns or rows)
      -> String -- ^ Encoded message
      -> From   -- ^ Parsed data structure
parse _ ('l' : t) = (\(a, _) -> a) (parseLL t)
parse _ _ = error "Invalid bencode"



convertList :: Int -> [(Int, Char)] -> To
convertList y ((x, sym) : t) = [(x, y, sym)] ++ convertList y t
convertList _ [] = []

convertLL :: Int -> From -> To
convertLL num (h : t) = (convertList num h) ++ (convertLL (num+1) t)
convertLL _ [] = []

convert :: Int  -- ^ Size of the matrix (number of columns or rows)
        -> From -- ^ Parsed matrix
        -> To   -- ^ Converted matrix
convert _ ll = convertLL 0 ll
