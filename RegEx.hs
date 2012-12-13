module RegEx where

import qualified Data.Map as Map
import Data.Maybe(fromMaybe)

import Debug.Trace
import SeriRegEx

instance (Show c) => Show (RegEx c) where
    show (Epsilon) = "Epsilon"
    show (Empty)   = "Empty"    
    show (Atom c)  = "Atom " ++ show c
    show (Range cmin cmax) = "Range " ++ show cmin ++ " " ++ show cmax
    show (Star x)      = "Star (" ++ show x ++ ")"
    show (Concat a b)  = "Concat (" ++ show a ++ ") (" ++ show b ++ ")"
    show (Or a b)      = "Or (" ++ show a ++ ") (" ++ show b ++ ")"
    show (Variable id) = "Variable " ++ show id
    show (Fix f n)     = "Fix (" ++ show f ++ ") " ++ show n
                         
charR :: (FromChar c) => Char -> RegEx c
charR c = Atom (fromChar c)

concatR :: RegEx c -> RegEx c -> RegEx c
concatR Empty          y = Empty
concatR Epsilon        y = y            
concatR (Concat x1 x2) y = x1 `concatR` (x2 `concatR` y )            
concatR x          Empty = Empty
concatR x        Epsilon = x
concatR x              y = Concat x y

concatsR :: [RegEx c] -> RegEx c
concatsR = foldl concatR epsilonR

orsR :: [RegEx c] -> RegEx c
orsR = foldl orR emptyR

orR :: RegEx c -> RegEx c -> RegEx c
orR Empty      y = y
orR (Or x1 x2) y = x1 `orR` (x2 `orR` y)           
orR x      Empty = x
orR x          y = Or x y                                    

epsilonR :: RegEx c
epsilonR = Epsilon

varR :: ID -> RegEx c
varR = Variable

fixR :: RegEx c -> Integer -> RegEx c
fixR = Fix

stringR :: (FromChar c) => String -> RegEx c
stringR str = foldr concatR Epsilon (map charR str)

starR :: RegEx c -> RegEx c
starR Epsilon = Epsilon
starR Empty   = Empty  
starR x       = Star x

plusR :: RegEx c -> RegEx c
plusR r = concatR r (starR r)

optionR :: RegEx c -> RegEx c
optionR r = orR epsilonR r

rangeR :: (FromChar c) => Char -> Char -> RegEx c
rangeR a b = Range (fromChar a) (fromChar b)

emptyR :: RegEx c
emptyR = Empty

fix :: Map.Map ID (RegEx c) -> ID -> Integer -> RegEx c
fix regs id n =
  let fix' :: Map.Map (ID, Integer) (RegEx c) -> Integer -> RegEx c -> RegEx c
      fix' m n r
        | Epsilon <- r = if n == 0 then Epsilon else Empty
        | Empty <- r = Empty
        | Atom {} <- r = if n == 1 then r else Empty
        | Range {} <- r = if n == 1 then r else Empty
        | Star x <- r = 
            if n == 0
                then Epsilon
                else fix' m n (Concat x r)
        | Concat a b <- r
            = orsR [concatR (fix' m i a) (fix' m (n-i) b) | i <- [0..n]]
        | Or a b <- r = orR (fix' m n a) (fix' m n b)
        | Variable id <- r = fromMaybe Empty (Map.lookup (id,n) m)
        | Fix x n' <- r = if n == n' then fix' m n x else Empty
    
      --fixid :: Map.Map (ID, Integer) (RegEx c) -> (ID, Integer) -> RegEx c
      fixid t k@(id, n) = 
        let t' = Map.insert k Empty t
            src = fromMaybe (error $ "fixid: " ++ show id) (Map.lookup id regs)
        in fix' t' n src

      keys = [(x, i) | i <- [0..n], x <- Map.keys regs]
      table = Map.fromList [(k, fixid table k) | k <- keys]
  in fromMaybe (error $ "fix: " ++ show id) (Map.lookup (id, n) table)

