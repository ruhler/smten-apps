
-- Data structure for manipulating rectangular blocks of data
module Block (
   Block, rows, cols, pdiags, ndiags,
   blockM, fromRows,
   blockMapM,
  ) where

import Smten.Prelude

-- A rectangular block of data organized as a list of rows.
-- We maintain the following invariants:
--   * the number of elements in each row is the same. 
type Block a = [[a]]

rows :: Block a -> [[a]]
rows = id

cols :: Block a -> [[a]]
cols [] = []
cols ([]:_) = []
cols xs = map head xs : cols (map tail xs)

-- Given a block, return the list of positive diagonals from upper right to
-- lower left.
-- For example, the result of pdiags on the block:
--  abc
--  def
--  ghi
-- is:  [a, bd, ceg, fh, i]
pdiags :: [[a]] -> [[a]]
pdiags [] = []
pdiags ([]:_) = []
pdiags [xs] = map (\x -> [x]) xs
pdiags ((a:as):b) =
  let fmerge [] y = y
      fmerge (x:xs) (y:ys) = (x:y) : fmerge xs ys
  in [a] : fmerge as (pdiags b)

-- Given a rectangular 2D array as a list of rows, return the list of negative
-- diagonals from upper left to lower right
-- For example, the result of ndiags on the block:
--  abc
--  def
--  ghi
-- is [c, bf, aei, dh, g]
ndiags :: [[a]] -> [[a]]
ndiags x = pdiags (map reverse x)

-- blockM numrows numcols
-- Construct a block of the given dimensions monadically
blockM :: (Monad m) => m a -> Int -> Int -> m (Block a)
blockM f nr nc =
  let mkrow = sequence $ replicate nc f
  in sequence $ replicate nr mkrow

fromRows :: [[a]] -> Block a
fromRows = id

blockMapM :: (Monad m) => (a -> m b) -> Block a -> m (Block b)
blockMapM f xs = mapM (mapM f) xs

