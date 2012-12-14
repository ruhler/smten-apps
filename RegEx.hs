
module RegEx (
    epsilonR, emptyR, concatR, orsR, orR,
    ) where

import SeriRegEx

concatR :: RegEx c -> RegEx c -> RegEx c
concatR Empty          y = Empty
concatR Epsilon        y = y            
concatR x          Empty = Empty
concatR x        Epsilon = x
concatR x              y = Concat (rlength x + rlength y) x y

orsR :: [RegEx c] -> RegEx c
orsR = foldl orR emptyR

orR :: RegEx c -> RegEx c -> RegEx c
orR Empty      y = y
orR x      Empty = x
orR x          y = Or (rlength x) x y                                    

epsilonR :: RegEx c
epsilonR = Epsilon

emptyR :: RegEx c
emptyR = Empty

