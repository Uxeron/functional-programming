module Task2Message
where

-- ┌         ┐
-- │ X X O O │
-- │ X O O O │
-- │ O X X O │
-- │ X X O X │
-- └         ┘
-- seed: -6631911740610304421
-- encoding: Ben
-- list entry: LIL
-- convert to: ARR

data JsonLikeValue = JLString String | JLInt Int | JLMap [(String, JsonLikeValue)] | JLArray [JsonLikeValue] deriving (Show, Eq)
data InvalidState = Order | Duplicates deriving (Show, Eq)

size :: Int
size = 4

message :: String
message = "d4:prevd4:prevd4:prevd4:lastlleleleld1:xi0e1:v1:Xeee4:prevd4:lastlld1:xi3e1:v1:Oeelelelee4:prevd4:lastlleleleld1:xi3e1:v1:Xeee4:prevd4:prevd4:lastlleleld1:xi2e1:v1:Xeelee4:prevd4:lastlleleld1:xi3e1:v1:Oeelee4:prevd4:lastlleleleld1:xi1e1:v1:Xeee4:prevd4:prevd4:lastlleld1:xi0e1:v1:Xeelelee4:prevd4:lastlleld1:xi3e1:v1:Oeelelee4:prevd4:lastlld1:xi1e1:v1:Xeelelelee4:prevd4:prevd4:lastlleleld1:xi1e1:v1:Xeeleee4:lastlleleleld1:xi2e1:v1:Oeeeeeee4:lastlleld1:xi2e1:v1:Oeeleleeeeee4:lastlleleld1:xi0e1:v1:Oeeleeeeee4:lastlld1:xi2e1:v1:Oeeleleleee4:lastlld1:xi0e1:v1:Xeeleleleee4:lastlleld1:xi1e1:v1:Oeeleleee"

message' :: String
message' = "d4:prevd4:prevd4:prevd4:lastlleleleld1:xi0e1:v1:Xeee4:prevd4:lastlld1:xi3e1:v1:Oeelelelee4:prevd4:lastlleleleld1:xi3e1:v1:Xeee4:prevd4:prevd4:lastlleleld1:xi2e1:v1:Xeelee4:prevd4:lastlleleld1:xi3e1:v1:Oeelee4:prevd4:lastlleleleld1:xi1e1:v1:Xeee4:prevd4:prevd4:lastlleld1:xi0e1:v1:Xeelelee4:prevd4:lastlleld1:xi3e1:v1:Oeelelee4:prevd4:lastlld1:xi1e1:v1:Xeelelelee4:prevd4:prevd4:lastlleleld1:xi1e1:v1:Xeeleee4:lastlleleleld1:xi2e1:v1:Oeeeeeee4:lastlleld1:xi2e1:v1:Oeeleleeeeee4:lastlleleld1:xi0e1:v1:Oeeleeeeee4:lastlld1:xi2e1:v1:Oeeleleleee4:lastlld1:xi0e1:v1:Xeeleleleee4:lastlleld1:xi1e1:v1:Oeeleleee"

type To = ([Int], [Int], [Char])

expectedParse :: Either String JsonLikeValue
expectedParse = Right $ JLMap [("prev", JLMap [("prev", JLMap [("prev", JLMap [("last", JLArray [JLArray [], JLArray [], JLArray [], JLArray [JLMap [("x", JLInt 0), ("v", JLString "X")]]]), ("prev", JLMap [("last", JLArray [JLArray [JLMap [("x", JLInt 3), ("v", JLString "O")]], JLArray [], JLArray [], JLArray []]), ("prev", JLMap [("last", JLArray [JLArray [], JLArray [], JLArray [], JLArray [JLMap [("x", JLInt 3), ("v", JLString "X")]]]), ("prev", JLMap [("prev", JLMap [("last", JLArray [JLArray [], JLArray [], JLArray [JLMap [("x", JLInt 2), ("v", JLString "X")]], JLArray []]), ("prev", JLMap [("last", JLArray [JLArray [], JLArray [], JLArray [JLMap [("x", JLInt 3), ("v", JLString "O")]], JLArray []]), ("prev", JLMap [("last", JLArray [JLArray [], JLArray [], JLArray [], JLArray [JLMap [("x", JLInt 1), ("v", JLString "X")]]]), ("prev", JLMap [("prev", JLMap [("last", JLArray [JLArray [], JLArray [JLMap [("x", JLInt 0), ("v", JLString "X")]], JLArray [], JLArray []]), ("prev", JLMap [("last", JLArray [JLArray [], JLArray [JLMap [("x", JLInt 3), ("v", JLString "O")]], JLArray [], JLArray []]), ("prev", JLMap [("last", JLArray [JLArray [JLMap [("x", JLInt 1), ("v", JLString "X")]], JLArray [], JLArray [], JLArray []]), ("prev", JLMap [("prev", JLMap [("last", JLArray [JLArray [], JLArray [], JLArray [JLMap [("x", JLInt 1), ("v", JLString "X")]], JLArray []])]), ("last", JLArray [JLArray [], JLArray [], JLArray [], JLArray [JLMap [("x", JLInt 2), ("v", JLString "O")]]])])])])]), ("last", JLArray [JLArray [], JLArray [JLMap [("x", JLInt 2), ("v", JLString "O")]], JLArray [], JLArray []])])])])]), ("last", JLArray [JLArray [], JLArray [], JLArray [JLMap [("x", JLInt 0), ("v", JLString "O")]], JLArray []])])])])]), ("last", JLArray [JLArray [JLMap [("x", JLInt 2), ("v", JLString "O")]], JLArray [], JLArray [], JLArray []])]), ("last", JLArray [JLArray [JLMap [("x", JLInt 0), ("v", JLString "X")]], JLArray [], JLArray [], JLArray []])]), ("last", JLArray [JLArray [], JLArray [JLMap [("x", JLInt 1), ("v", JLString "O")]], JLArray [], JLArray []])]

expectedConvert :: Either InvalidState To
expectedConvert = Right ([0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 3], [0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3], ['X', 'X', 'O', 'O', 'X', 'O', 'O', 'O', 'O', 'X', 'X', 'O', 'X', 'X', 'O', 'X'])
