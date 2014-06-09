
module Bool2NQueens (bool2_nqueens) where

import Smten.Prelude
import Smten.Search

import Block

type Placement = Block Bool

-- Assert only one bool is set of the given ones by pairwise asserting
-- no two elements are set.
noconf :: [Bool] -> Bool
noconf [] = True
noconf (x:xs) = 
  let nc a b = (not a || not b)
  in all (nc x) xs && noconf xs

islegal :: Placement -> Bool
islegal places = and [
  {-# SCC "RowsOccupied" #-}    all or (rows places),       -- each row must have at least one queen
  {-# SCC "RowsNonConflict" #-} all noconf (rows places),   -- rows shouldn't conflict
  {-# SCC "ColsNonConflict" #-} all noconf (cols places),   -- columns shouldn't conflict
  {-# SCC "PDNonConflict" #-}   all noconf (pdiags places), -- diagonals shouldn't conflict
  {-# SCC "NDNonConflict" #-}   all noconf (ndiags places)]

bool2_nqueens :: Int -> Space [Int]
bool2_nqueens n = do
  places <- blockM free_Bool n n
  guard (islegal places)
  return $ map colof (rows places)

colof :: [Bool] -> Int
colof [] = error "no queen found in given row"
colof (True:_) = 0
colof (False:xs) = 1 + colof xs


