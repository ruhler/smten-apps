
module RegEx (
    epsilonR, emptyR, concatR, orsR, orR,
    ) where

import SeriRegEx

concatR :: RegEx c -> RegEx c -> RegEx c
concatR Empty          y = Empty
concatR Epsilon        y = y            
concatR (Concat x1 x2) y = x1 `concatR` (x2 `concatR` y )            
concatR x          Empty = Empty
concatR x        Epsilon = x
concatR x              y = Concat x y

orsR :: [RegEx c] -> RegEx c
orsR = foldl orR emptyR

orR :: RegEx c -> RegEx c -> RegEx c
orR Empty      y = y
orR (Or x1 x2) y = x1 `orR` (x2 `orR` y)           
orR x      Empty = x
orR x          y = Or x y                                    

epsilonR :: RegEx c
epsilonR = Epsilon

emptyR :: RegEx c
emptyR = Empty

