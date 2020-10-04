module Task1 where

import Task1Message

import Data.Char

parseDict :: String -> ((Int, Char), String)
parseDict ('1':':':'v':'1':':' : sym : '1':':':'x':'1':':' : x : 'e' : t) = ((digitToInt x, sym), t)


parseList :: String -> ([(Int, Char)], String)
parseList ('d' : t) = 
      let
            (val, str) = parseDict t
            (list, str1) = parseList str
      in
            (val : list, str1)

parseList ('e' : t) = ([], t)


parseLL :: String -> ([[(Int, Char)]], String)
parseLL ('l' : t) = 
      let 
            (list, str) = parseList t
            (ll, str1) = parseLL str
      in
            (list : ll, str1)

parseLL ('e' : t) = ([], t)


parse :: Int    -- ^ Size of the matrix (number of columns or rows)
      -> String -- ^ Encoded message
      -> From   -- ^ Parsed data structure
parse _ ('l' : t) =  
      let
            (ll, _) = parseLL t
      in
            ll



convert :: Int  -- ^ Size of the matrix (number of columns or rows)
        -> From -- ^ Parsed matrix
        -> To   -- ^ Converted matrix
convert _ _ = expectedTo 
