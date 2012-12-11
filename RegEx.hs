
module RegEx (
    RegEx(), match,
    charR, stringR, starR
    ) where

data RegEx = Empty
           | Atom Char Char
           | Star RegEx
           | Concat RegEx RegEx
           | Or [RegEx]
    deriving (Eq, Show)

-- Find all possible paritions for the given string.
partitions :: String -> [(String, String)]
partitions str = [splitAt i str | i <- [0..(length str)]]

match :: RegEx -> String -> Bool
match Empty str = null str
match (Atom cmin cmax) [c] = cmin <= c && c <= cmax
match (Atom {}) _ = False
match s@(Star x) str = match (Or [Empty, Concat x s]) str
match (Concat a b) str
  = or [match a sa && match b sb | (sa, sb) <- partitions str]
match (Or rs) str = or [match r str | r <- rs]

charR :: Char -> RegEx
charR c = Atom c c

concatR :: RegEx -> RegEx -> RegEx
concatR r Empty = r
concatR a b = Concat a b

stringR :: String -> RegEx
stringR str = foldr concatR Empty (map charR str)

starR :: RegEx -> RegEx
starR = Star

