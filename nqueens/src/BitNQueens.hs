
{-# LANGUAGE DataKinds #-}

-- | Implementation of nqueens based on the Bit-vector type.
module BitNQueens( bit_nqueens ) where

import Smten.Prelude
import Smten.Data.Bit
import Smten.Symbolic

type Bits = Bit 12

-- Placement: A list of locations (row, col) for each queen.
-- Indices go from 0 to n-1
type Placement = [(Bits, Bits)]

distinct :: (Eq a) => [a] -> Bool
distinct [] = True
distinct (x:xs) = x `notElem` xs && distinct xs

islegal :: Placement -> Bool
islegal places = and [
  distinct (map fst places),
  distinct (map snd places),
  distinct (map (uncurry (+)) places),
  distinct (map (uncurry (-)) places)]

mkcol :: Int -> Symbolic Bits
mkcol n = do
    x <- free_Bit
    assert (x < toEnum n)
    return x

supported :: Int -> Bool
supported n = 2*n < fromEnum (bv_not 0 :: Bits)

bit_nqueens :: Int -> Symbolic [Int]
bit_nqueens n
 | supported n = do
      let rows = [0..(toEnum n-1)]
      cols <- sequence $ replicate n (mkcol n)
      let places = zip rows cols
      assert (islegal places)
      return $ map fromEnum cols
 | otherwise = error ("n = " ++ show n ++ " not supported. Increase bit width")

