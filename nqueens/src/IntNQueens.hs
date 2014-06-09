
-- | Implementation of nqueens based on the Int type.
module IntNQueens( int_nqueens ) where

import Smten.Prelude
import Smten.Control.Monad
import Smten.Search

-- Placement: A list of locations (row, col) for each queen.
-- Indices go from 0 to n-1
type Queen = (Int, Int)
type Queens = [Queen]

distinct :: [Int] -> Bool
distinct [] = True
distinct (x:xs) = x `notElem` xs && distinct xs

islegal :: Queens -> Bool
islegal queens = and [
  distinct (map fst queens),
  distinct (map snd queens),
  distinct (map (uncurry (+)) queens),
  distinct (map (uncurry (-)) queens)]

mkcol :: Int -> Space Int
mkcol n = msum (map return [0..(n-1)])

int_nqueens :: Int -> Space [Int]
int_nqueens n = do
    let rows = [0..(n-1)]
    cols <- replicateM n (mkcol n)
    let queens = zip rows cols
    guard (islegal queens)
    return cols

