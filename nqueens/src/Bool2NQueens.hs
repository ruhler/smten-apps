
module Bool2NQueens (bool2_nqueens) where

import Smten.Prelude
import Smten.Symbolic

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
  all or (rows places),     -- each row must have at least one queen
  all noconf (rows places), -- rows shouldn't conflict
  all noconf (cols places), -- columns shouldn't conflict
  all noconf (pdiags places),   -- diagonals shouldn't conflict
  all noconf (ndiags places)]

bool2_nqueens :: Int -> Symbolic [Int]
bool2_nqueens n = do
  places <- blockM free_Bool n n
  assert (islegal places)
  return $ map colof (rows places)

colof :: [Bool] -> Int
colof [] = error "no queen found in given row"
colof (True:_) = 0
colof (False:xs) = 1 + colof xs


