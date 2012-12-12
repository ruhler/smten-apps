
module RegEx where

import Prelude

type ID = String

data RegEx c = Epsilon
           | Atom c
           | Range c c
           | Star (RegEx c)
           | Concat (RegEx c) (RegEx c)
           | Or [RegEx c]
           | Variable ID
           | Fix (RegEx c) Integer

instance (Show c) => Show (RegEx c) where
    show (Epsilon) = "Epsilon"
    show (Atom c) = "Atom " ++ show c
    show (Range cmin cmax) = "Range " ++ show cmin ++ " " ++ show cmax
    show (Star x) = "Star (" ++ show x ++ ")"
    show (Concat a b) = "Concat (" ++ show a ++ ") (" ++ show b ++ ")"
    show (Or xs) = "Or " ++ show xs
    show (Variable id) = "Variable " ++ show id

class FromChar c where
    fromChar :: Char -> c

instance FromChar Char where
    fromChar = id

ilength :: [a] -> Integer
ilength (x:xs) = 1 + ilength xs
ilength _ = 0


partitions :: [a] -> [([a], [a])]
partitions str = map (flip splitAt str) [0..(length str)]

match :: (Eq c, Ord c) => RegEx c -> [c] -> Bool
match Epsilon str = null str
match (Atom x) [c] = x == c
match (Atom _) _ = False
match (Range cmin cmax) [c] = cmin <= c && c <= cmax
match (Range _ _) _ = False
match s@(Star x) str = match (Or [Epsilon, Concat x s]) str
match (Concat a b) str = any (matchboth a b) (partitions str)
match (Or rs) str = any (flip match str) rs
match (Fix x n) str = (ilength str == n) && match x str
match (Variable x) _ = error $ "match: Variable " ++ x

matchboth :: (Eq c, Ord c) => RegEx c -> RegEx c -> ([c], [c]) -> Bool
matchboth a b (sa, sb) = match a sa && match b sb

charR :: (FromChar c) => Char -> RegEx c
charR c = Atom (fromChar c)

concatR :: RegEx c -> RegEx c -> RegEx c
concatR Epsilon r = r
concatR r Epsilon = r
concatR a b = Concat a b

concatsR :: [RegEx c] -> RegEx c
concatsR = foldl concatR epsilonR

orR :: [RegEx c] -> RegEx c
orR = Or

epsilonR :: RegEx c
epsilonR = Epsilon

varR :: ID -> RegEx c
varR = Variable

fixR :: RegEx c -> Integer -> RegEx c
fixR = Fix

stringR :: (FromChar c) => String -> RegEx c
stringR str = foldr concatR Epsilon (map charR str)

starR :: RegEx c -> RegEx c
starR = Star

plusR :: RegEx c -> RegEx c
plusR r = concatR r (starR r)

optionR :: RegEx c -> RegEx c
optionR r = orR [epsilonR, r]

rangeR :: (FromChar c) => Char -> Char -> RegEx c
rangeR a b = Range (fromChar a) (fromChar b)

