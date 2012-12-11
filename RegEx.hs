
module RegEx
  --(RegEx(), match, charR, stringR, starR)
  where

data RegEx c = Empty
           | Atom c c
           | Star (RegEx c)
           | Concat (RegEx c) (RegEx c)
           | Or [RegEx c]
    deriving (Eq, Show)

class FromChar c where
    fromChar :: Char -> c

instance FromChar Char where
    fromChar = id

-- Find all possible paritions for the given list.
partitions :: [a] -> [([a], [a])]
partitions str = map (flip splitAt str) [0..(length str)]

match :: (Ord c) => RegEx c -> [c] -> Bool
match Empty str = null str
match (Atom cmin cmax) [c] = cmin <= c && c <= cmax
match (Atom {}) _ = False
match s@(Star x) str = match (Or [Empty, Concat x s]) str
match (Concat a b) str = any (matchboth a b) (partitions str)
match (Or rs) str = any (flip match str) rs

matchboth :: (Ord c) => RegEx c -> RegEx c -> ([c], [c]) -> Bool
matchboth a b (sa, sb) = match a sa && match b sb

charR :: (FromChar c) => Char -> RegEx c
charR c = Atom (fromChar c) (fromChar c)

concatR :: RegEx c -> RegEx c -> RegEx c
concatR r Empty = r
concatR a b = Concat a b

stringR :: (FromChar c) => String -> RegEx c
stringR str = foldr concatR Empty (map charR str)

starR :: RegEx c -> RegEx c
starR = Star

