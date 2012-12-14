
-- This file shared by Haskell and Seri.
-- So it should be restricted to Seri syntax and features.
module SeriRegEx where

import Prelude

data RegEx c =
             Epsilon        -- matches ""
           | Empty          -- never matches
           | Atom c
           | Range c c
           | Star (RegEx c)
           | Concat (RegEx c) (RegEx c)
           | Or (RegEx c) (RegEx c)
   deriving(Eq)

partitions :: [a] -> [([a], [a])]
partitions str = map (flip splitAt str) [0..(length str)]

match :: (Eq c, Ord c) => RegEx c -> [c] -> Bool
match Epsilon str = null str
match Empty _ = False
match (Atom x) [c] = x == c
match (Atom _) _ = False
match (Range cmin cmax) [c] = cmin <= c && c <= cmax
match (Range _ _) _ = False
match s@(Star x) str = match (Or Epsilon (Concat x s)) str
match (Concat a b) str = any (matchboth a b) (partitions str)
match (Or a b) str = match a str || match b str

matchboth :: (Eq c, Ord c) => RegEx c -> RegEx c -> ([c], [c]) -> Bool
matchboth a b (sa, sb) = match a sa && match b sb

