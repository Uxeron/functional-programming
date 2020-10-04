module Task1 where

import Task1Message

parse :: Int    -- ^ Size of the matrix (number of columns or rows)
      -> String -- ^ Encoded message
      -> From   -- ^ Parsed data structure
parse _ _ = expectedFrom

convert :: Int  -- ^ Size of the matrix (number of columns or rows)
        -> From -- ^ Parsed matrix
        -> To   -- ^ Converted matrix
convert _ _ = expectedTo 
