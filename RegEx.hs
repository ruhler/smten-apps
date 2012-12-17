
{-# LANGUAGE StandaloneDeriving #-}

module RegEx (
    epsilonR, emptyR, concatR, orsR,
    ) where

import Data.List(nub)

import SeriRegEx

instance Show RegEx where
    show Epsilon = "ϵ"
    show Empty = "∅"
    show (Atom c) = show c
    show (Range a b) = "[" ++ show a ++ "-" ++ show b ++ "]"
    show (Concat _ a b) = show a ++ " " ++ show b
    show (Or _ a b) = "(" ++ show a ++ " | " ++ show b ++ ")"

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

