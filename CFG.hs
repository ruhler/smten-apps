
module CFG where

import Elem

type ID = String

data CFG =
     EpsilonC       -- matches ""
   | EmptyC         -- never matches
   | AtomC Elem
   | RangeC Elem Elem
   | StarC CFG
   | ConcatC CFG CFG
   | OrC CFG CFG
   | VariableC ID
   | FixC CFG Integer
   deriving(Show, Eq)

charC :: Char -> CFG
charC c = AtomC (fromChar c)

concatC :: CFG -> CFG -> CFG
concatC EmptyC          y = EmptyC
concatC EpsilonC        y = y            
concatC (ConcatC x1 x2) y = x1 `concatC` (x2 `concatC` y )            
concatC x          EmptyC = EmptyC
concatC x        EpsilonC = x
concatC x              y = ConcatC x y

concatsC :: [CFG] -> CFG
concatsC = foldl concatC epsilonC

orsC :: [CFG] -> CFG
orsC = foldl orC emptyC

orC :: CFG -> CFG -> CFG
orC EmptyC      y = y
orC (OrC x1 x2) y = x1 `orC` (x2 `orC` y)           
orC x      EmptyC = x
orC x          y = OrC x y                                    

epsilonC :: CFG
epsilonC = EpsilonC

varC :: ID -> CFG
varC = VariableC

fixC :: CFG -> Integer -> CFG
fixC = FixC

stringC :: String -> CFG
stringC str = foldr concatC EpsilonC (map charC str)

starC :: CFG -> CFG
starC EpsilonC = EpsilonC
starC EmptyC   = EmptyC
starC x       = StarC x

plusC :: CFG -> CFG
plusC r = concatC r (starC r)

optionC :: CFG -> CFG
optionC r = orC epsilonC r

rangeC :: Char -> Char -> CFG
rangeC a b = RangeC (fromChar a) (fromChar b)

emptyC :: CFG
emptyC = EmptyC

