
-- | Implementation of nqueens based on the Int type.
module IntNQueens( int_nqueens ) where

import Smten.Prelude
import Smten.Control.Monad
import Smten.Symbolic

-- Placement: A list of locations (row, col) for each queen.
-- Indices go from 0 to n-1
type Placement = [(Int, Int)]

distinct :: (Eq a) => [a] -> Bool
distinct [] = True
distinct (x:xs) = x `notElem` xs && distinct xs

islegal :: Placement -> Bool
islegal places = and [
  distinct (map fst places),
  distinct (map snd places),
  distinct (map (uncurry (+)) places),
  distinct (map (uncurry (-)) places)]

mkcol :: Int -> Symbolic Int
mkcol n = msum (map return [0..(n-1)])

int_nqueens :: Int -> Symbolic [Int]
int_nqueens n = do
    let rows = [0..(n-1)]
    cols <- sequence $ replicate n (mkcol n)
    let places = zip rows cols
    assert (islegal places)
    return cols

