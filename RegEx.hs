
module RegEx where

import SeriRegEx

instance (Show c) => Show (RegEx c) where
    show (Epsilon) = "Epsilon"
    show (Empty)   = "Empty"    
    show (Atom c)  = "Atom " ++ show c
    show (Range cmin cmax) = "Range " ++ show cmin ++ " " ++ show cmax
    show (Star x) = "Star (" ++ show x ++ ")"
    show (Concat a b) = "Concat (" ++ show a ++ ") (" ++ show b ++ ")"
    show (Or a b) = "Or (" ++ show a ++ ") (" ++ show b ++ ")"
    show (Variable id) = "Variable " ++ show id

charR :: (FromChar c) => Char -> RegEx c
charR c = Atom (fromChar c)

concatR :: (Eq c) => RegEx c -> RegEx c -> RegEx c
concatR x y | x == Empty   = Empty
            | x == Epsilon = y            
            | (Concat x1 x2) <- x = x1 `concatR` (x2 `concatR` y )            
            | y == Empty   = Empty
            | y == Epsilon = x
            | otherwise    = Concat x y                           

concatsR :: (Eq c) => [RegEx c] -> RegEx c
concatsR = foldl concatR epsilonR

orsR :: (Eq c) => [RegEx c] -> RegEx c
orsR = foldl orR emptyR

orR :: (Eq c) => RegEx c -> RegEx c -> RegEx c
orR x y | x == Empty = y
        | (Or x1 x2) <- x = x1 `orR` (x2 `orR` y)           
        | y == Empty      = x
        | x == y          = x
        | otherwise       = Or x y                                    

epsilonR :: RegEx c
epsilonR = Epsilon

varR :: ID -> RegEx c
varR = Variable

fixR :: RegEx c -> Integer -> RegEx c
fixR = Fix

stringR :: (Eq c, FromChar c) => String -> RegEx c
stringR str = foldr concatR Epsilon (map charR str)

starR :: RegEx c -> RegEx c
starR = Star

plusR :: (Eq c) => RegEx c -> RegEx c
plusR r = concatR r (starR r)

optionR :: (Eq c) => RegEx c -> RegEx c
optionR r = orR epsilonR r

rangeR :: (FromChar c) => Char -> Char -> RegEx c
rangeR a b = Range (fromChar a) (fromChar b)

emptyR :: RegEx c
emptyR = Empty

-------------------------------------------------

matchEpsilon :: [(ID, RegEx c)] -> RegEx c -> Bool
matchEpsilon env          Empty = False
matchEpsilon env        Epsilon = True
matchEpsilon env       (Atom _) = False
matchEpsilon env     (Range {}) = False
matchEpsilon env       (Star _) = True
matchEpsilon env (Concat c1 c2) = matchEpsilon env c1 && matchEpsilon env c2
matchEpsilon env     (Or c1 c2) = matchEpsilon env c1 || matchEpsilon env c2
matchEpsilon env   (Variable i) = case lookup i env of
                                    Nothing  -> False
                                    (Just c) -> matchEpsilon (filter (\(x,_) -> x /= i) env) c -- okay for match epsilon
matchEpsilon env      (Fix r n) = n == 0 && matchEpsilon env r

fixNonZero :: (Eq c) => [(ID, RegEx c)] -> RegEx c -> RegEx c
fixNonZero env c | not (matchEpsilon env c) = c
fixNonZero env Empty            = Empty
fixNonZero env Epsilon          = Empty
fixNonZero env c@(Atom {})      = c
fixNonZero env c@(Range {})     = c
fixNonZero env c@(Star cb)      = concatR (fixNonZero env cb) c
fixNonZero env c@(Concat c1 c2) = case (matchEpsilon env c1, matchEpsilon env c2) of
                                      (True, True) -> orsR [fixNonZero env c1, fixNonZero env c2,
                                                               concatR (fixNonZero env c1) (fixNonZero env c2)]
                                      _            -> c
fixNonZero env c@(Or c1 c2)     = Or (fixNonZero env c1) (fixNonZero env c2)
fixNonZero env (Variable i)          = case lookup i env of
                                      Nothing  -> Empty
                                      (Just c) -> fixNonZero env c
fixNonZero env c@(Fix _ i)        = if i == 0 then Empty else c

fixTable :: (Eq c) => [(ID, RegEx c)] -> Integer -> [((Integer,ID), RegEx c)]
fixTable env n = table
  where keys = [(i,v, c) | i <- [0..n], (v,c) <- env]
        table = foldl (\t (n,i,c) -> ((n,i),fix' t n c):t) [] keys
        fix' t 0 cfg         = if matchEpsilon env cfg then Epsilon else Empty        
        fix' t n Empty       = Empty
        fix' t n Epsilon     = if n == 0 then Epsilon else Empty 
        fix' t n c@(Atom {}) = if n == 1 then c else Empty
        fix' t n c@(Range{}) = if n == 1 then c else Empty        
        fix' t n c@(Star cb) = if n == 0 then Epsilon else fix' t n (concatR (fixNonZero env cb) c)
        fix' t n c@(Concat c1 c2) = orsR $ map (\x -> concatR (fix' t x c1) (fix' t (n-x) c2)) [0..n]
        fix' t n (Or c1 c2)    = orR (fix' t n c1) (fix' t n c2)        
        fix' t n c@(Variable i)     = case lookup (n, i) t of
                                      Just  x -> x
                                      Nothing -> Empty
        fix' t n (Fix c n')      = if n /= n' then Empty else fix' t n c

