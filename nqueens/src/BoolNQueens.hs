
module BoolNQueens (bool_nqueens) where

import Smten.Prelude
import Smten.Symbolic

import Block

type Placement = Block Bool

-- Return True if exactly one of the inputs is set.
oneset :: [Bool] -> Bool
oneset =
  -- s - at least some bit has been seen set.
  -- m - multiple bits have been seen set.
  let oneset' s m [] = s && not m
      oneset' s m (x:xs) = oneset' (s || x) ((s && x) || m) xs
  in oneset' False False

-- Return True if multiple inputs are set
multiset :: [Bool] -> Bool
multiset =
  -- s - at least some bit has been seen set.
  -- m - multiple bits have been seen set.
  let multiset' s m [] = m
      multiset' s m (x:xs) = multiset' (s || x) ((s && x) || m) xs
  in multiset' False False
 
islegal :: Placement -> Bool
islegal places = and [
  all oneset (rows places),
  all oneset (cols places),
  all (not . multiset) (pdiags places),
  all (not . multiset) (ndiags places)]

bool_nqueens :: Int -> Symbolic [Int]
bool_nqueens n = do
  places <- blockM free_Bool n n
  assert (islegal places)
  return $ map colof (rows places)

colof :: [Bool] -> Int
colof [] = error "no queen found in given row"
colof (True:_) = 0
colof (False:xs) = 1 + colof xs


