module Task2 where

import Task2Message

import Data.List as L
import Data.Char as C


parseInt :: String -> Either (String, Int) (Int, String)
parseInt ('i' : t) = 
    case postfix of
        ('e' : r) | (length prefix) > 0 -> Right (read prefix, r)
        otherwise -> Left ("Invalid integer", length t)
    where
        prefix = L.takeWhile C.isDigit t
        postfix = drop (length prefix) t

parseInt t = Left ("Invalid integer", length t)


parseSingleMoveDict :: String -> Either (String, Int) (JsonLikeValue, String)
parseSingleMoveDict ('1':':':'v':'1':':' : sym : '1':':':'x': t) = 
    case parseInt t of
        Right (x, t') -> case t' of
            ('e' : t'') -> Right (JLMap [("v", JLString [sym]), ("x", JLInt x)], t'')
            otherwise -> Left ("Invalid single move dictionary", length t)
        Left err -> Left err

parseSingleMoveDict ('1':':':'x':t) = 
    case parseInt t of
        Right (x, t') -> case t' of
            ('1':':':'v':'1':':' : sym : 'e' : t'') -> Right (JLMap [("x", JLInt x), ("v", JLString [sym])], t'')
            otherwise -> Left ("Invalid single move dictionary", length t)
        Left err -> Left err

parseSingleMoveDict t = Left ("Invalid single move dictionary", length t)


parseList :: String -> Either (String, Int) (JsonLikeValue, String)
parseList ('d' : t) = 
    case parseSingleMoveDict t of
        Right (val, 'e' : t') -> Right (JLArray [val], t')
        Right _ -> Left ("Invalid list", length t)
        Left err -> Left err

parseList ('e' : t) = Right (JLArray [], t)
parseList t = Left ("Invalid list", length t)


parseLL :: String -> Either (String, Int) ([JsonLikeValue], String)
parseLL ('l' : t) = 
    case parseList t of
        Right (list, t') -> case parseLL t' of
            Right (ll, t'') -> Right ((list : ll), t'')
            Left err -> Left err 
        Left err -> Left err

parseLL ('e' : t) = Right ([], t)
parseLL t = Left ("Invalid list of lists", length t)


parseDictLast :: String -> Either (String, Int) ((String, JsonLikeValue), String)
parseDictLast ('l' : t) = 
    case parseLL t of
        Right (last, t') -> Right (("last", JLArray last), t')
        Left err -> Left err

parseDictLast ('4':':':'l':'a':'s':'t' : t) = parseDictLast t
parseDictLast t = Left ("Invalid dictionary Last", length t)


parseDictPrev :: String -> Either (String, Int) ((String, JsonLikeValue), String)
parseDictPrev ('d' : t) = 
    case parseDict t of
        Right (prev, t') -> Right (("prev", prev), t')
        Left err -> Left err

parseDictPrev ('4':':':'p':'r':'e':'v' : t) = parseDictPrev t
parseDictPrev t = Left ("Invalid dictionary Previous", length t)


parseDict :: String -> Either (String, Int) (JsonLikeValue, String)
parseDict ('d' : t) = parseDict t
parseDict ('4':':':'l':'a':'s':'t' : t) = 
    case parseDictLast t of
        Right (last, t') -> case t' of
            ('e' : t''') -> Right (JLMap [last], t''')
            otherwise -> case parseDictPrev t' of
                Right (prev, ('e' : t'')) -> Right (JLMap [last, prev], t'')
                Right _ -> Left ("Invalid dictionary", length t)
                Left err -> Left err
        Left err -> Left err

parseDict ('4':':':'p':'r':'e':'v' : t) = 
    case parseDictPrev t of
        Right (prev, t') -> case parseDictLast t' of
            Right (last, ('e' : t'')) -> Right (JLMap [prev, last], t'')
            Right _ -> Left ("Invalid dictionary", length t)
            Left err -> Left err
        Left err -> Left err

parseDict ('e' : t) = Right (JLArray [], t)
parseDict t = Left ("Invalid dictionary", length t)


parse :: Int                         -- ^ Size of the matrix (number of columns or rows)
      -> String                      -- ^ Encoded message
      -> Either String JsonLikeValue -- ^ Parsed data structure or error message
parse _ msg = 
    case parseDict msg of
        Right (val, _) -> Right val
        Left (err, pos) -> Left (err ++ " at position " ++ show (length msg - pos))

parse _ _ = expectedParse


convert :: Int                      -- ^ Size of the matrix (number of columns or rows)
        -> JsonLikeValue            -- ^ Parsed non-empty list of matrices
        -> Either InvalidState To   -- ^ Converted matrix
convert _ _ = expectedConvert

