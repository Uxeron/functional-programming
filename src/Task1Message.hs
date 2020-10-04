module Task1Message
where

-- ┌           ┐
-- │ O X X X O │
-- │ O X O X O │
-- │ X O X O X │
-- │ O O X O X │
-- │ X X O O X │
-- └           ┘
-- seed: -4328008003983893421
-- encoding: Ben
-- from: LIL
-- to: COO

size :: Int
size = 5

message :: String
message = "lld1:v1:O1:x1:0ed1:v1:X1:x1:1ed1:v1:X1:x1:2ed1:v1:X1:x1:3ed1:v1:O1:x1:4eeld1:v1:O1:x1:0ed1:v1:X1:x1:1ed1:v1:O1:x1:2ed1:v1:X1:x1:3ed1:v1:O1:x1:4eeld1:v1:X1:x1:0ed1:v1:O1:x1:1ed1:v1:X1:x1:2ed1:v1:O1:x1:3ed1:v1:X1:x1:4eeld1:v1:O1:x1:0ed1:v1:O1:x1:1ed1:v1:X1:x1:2ed1:v1:O1:x1:3ed1:v1:X1:x1:4eeld1:v1:X1:x1:0ed1:v1:X1:x1:1ed1:v1:O1:x1:2ed1:v1:O1:x1:3ed1:v1:X1:x1:4eee"

type From = [[(Int, Char)]]
type To = [(Int, Int, Char)]

expectedFrom :: From
expectedFrom = [[(0, 'O'), (1, 'X'), (2, 'X'), (3, 'X'), (4, 'O')], [(0, 'O'), (1, 'X'), (2, 'O'), (3, 'X'), (4, 'O')], [(0, 'X'), (1, 'O'), (2, 'X'), (3, 'O'), (4, 'X')], [(0, 'O'), (1, 'O'), (2, 'X'), (3, 'O'), (4, 'X')], [(0, 'X'), (1, 'X'), (2, 'O'), (3, 'O'), (4, 'X')]]

expectedTo :: To
expectedTo = [(0, 0, 'O'), (1, 0, 'X'), (2, 0, 'X'), (3, 0, 'X'), (4, 0, 'O'), (0, 1, 'O'), (1, 1, 'X'), (2, 1, 'O'), (3, 1, 'X'), (4, 1, 'O'), (0, 2, 'X'), (1, 2, 'O'), (2, 2, 'X'), (3, 2, 'O'), (4, 2, 'X'), (0, 3, 'O'), (1, 3, 'O'), (2, 3, 'X'), (3, 3, 'O'), (4, 3, 'X'), (0, 4, 'X'), (1, 4, 'X'), (2, 4, 'O'), (3, 4, 'O'), (4, 4, 'X')]
