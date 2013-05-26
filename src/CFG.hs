
module CFG where

import Data.Char

type ID = String

data CFG =
     EpsilonC       -- matches ""
   | EmptyC         -- never matches
   | AtomC Char
   | RangeC Char Char
   | StarC CFG
   | ConcatC CFG CFG
   | OrC CFG CFG
   | VariableC ID
   | FixC ID Integer
   deriving (Eq, Show)


charC :: Char -> CFG
charC = AtomC

concatC :: CFG -> CFG -> CFG
concatC EmptyC          y = EmptyC
concatC EpsilonC        y = y            
concatC (ConcatC x1 x2) y = x1 `concatC` (x2 `concatC` y )            
concatC x          EmptyC = EmptyC
concatC x        EpsilonC = x
concatC x              y = ConcatC x y

concatsC :: [CFG] -> CFG
concatsC = foldr concatC epsilonC

orsC :: [CFG] -> CFG
orsC = foldr orC emptyC

orC :: CFG -> CFG -> CFG
orC EmptyC      y = y

-- These are specifically for the WSU test cases, which often have long ranges
-- of characters explicitly spelled out.
orC (AtomC a) (AtomC b) | (ord a + 1) == ord b = rangeC a b
orC (AtomC a) (RangeC b c) | (ord a + 1) == ord b = rangeC a c
orC (AtomC a) (OrC (AtomC b) c) | (ord a + 1) == ord b = OrC (RangeC a b) c
orC (AtomC a) (OrC (RangeC b c) d) | (ord a + 1) == ord b = OrC (RangeC a c) d

orC (OrC x1 x2) y = x1 `orC` (x2 `orC` y)           
orC x      EmptyC = x
orC x          y = OrC x y                                    

epsilonC :: CFG
epsilonC = EpsilonC

varC :: ID -> CFG
varC = VariableC

fixC :: ID -> Integer -> CFG
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
rangeC = RangeC

emptyC :: CFG
emptyC = EmptyC

