
-- This file shared by Haskell and Seri.
-- So it should be restricted to Seri syntax and features.
module RegEx where

import Data.List

type RID = Integer
type Elem = Integer
type ElemString = [Elem]

data RegEx =
           Epsilon        -- matches ""
         | Empty          -- never matches
         | Atom Elem
         | Range Elem Elem
         | Concat Integer RegEx RegEx
         | Or Integer RegEx RegEx
         | Variable Integer RID
   deriving(Eq)

-- The length a string much be to match against the given regular expression.
rlength :: RegEx -> Integer
rlength Epsilon = 0
rlength Empty = 1   -- not strictly correct, what should go here?
rlength (Atom _) = 1
rlength (Range _ _) = 1
rlength (Concat n _ _) = n
rlength (Or n _ _) = n
rlength (Variable n _) = n

instance Show RegEx where
    show Epsilon = "ϵ"
    show Empty = "∅"
    show (Atom c) = show c
    show (Range a b) = "[" ++ show a ++ "-" ++ show b ++ "]"
    show (Concat _ a b) = show a ++ " " ++ show b
    show (Or _ a b) = "(" ++ show a ++ " | " ++ show b ++ ")"
    show (Variable n x) = show x ++ "." ++ show n

concatR :: RegEx -> RegEx -> RegEx
concatR Empty          y = Empty
concatR Epsilon        y = y            
concatR x          Empty = Empty
concatR x        Epsilon = x
concatR (Concat _ a b) y = concatR a (concatR b y)
concatR x              y = Concat (rlength x + rlength y) x y

orsR :: [RegEx] -> RegEx
orsR = foldl orR emptyR . nub

orR :: RegEx -> RegEx -> RegEx
orR Empty      y = y
orR x      Empty = x
orR (Or _ a b) y = orR a (orR b y)
orR x          y = Or (rlength x) x y                                    

epsilonR :: RegEx
epsilonR = Epsilon

emptyR :: RegEx
emptyR = Empty

