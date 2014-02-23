

-- | Implementation of nqueens based on the Integer type.
module IntegerNQueens( integer_nqueens ) where

import Smten.Prelude
import Smten.Symbolic

-- Placement: A list of locations (row, col) for each queen.
-- Indices go from 0 to n-1
type Placement = [(Integer, Integer)]

distinct :: (Eq a) => [a] -> Bool
distinct [] = True
distinct (x:xs) = x `notElem` xs && distinct xs

islegal :: Placement -> Bool
islegal places = and [
  distinct (map fst places),
  distinct (map snd places),
  distinct (map (uncurry (+)) places),
  distinct (map (uncurry (-)) places)]

mkcol :: Int -> Symbolic Integer
mkcol n = do
   x <- free_Integer
   assert (x >= 0 && x < toInteger n)
   return x

integer_nqueens :: Int -> Symbolic [Int]
integer_nqueens n = do
    let rows = [0..(toInteger n-1)]
    cols <- sequence $ replicate n (mkcol n)
    let places = zip rows cols
    assert (islegal places)
    return (map fromInteger cols)

