
{-# LANGUAGE DataKinds #-}

-- | Implementation of nqueens based on the Bit-vector type.
module BitNQueens( bit_nqueens ) where

import Smten.Prelude
import Smten.Control.Monad
import Smten.Control.Monad.Trans
import Smten.Data.Bit
import Smten.Symbolic
import Smten.Symbolic.SMT

-- Placement: A list of locations (row, col) for each queen.
-- Indices go from 0 to n-1
type Placement = [(Bit 8, Bit 8)]

distinct :: (Eq a) => [a] -> Bool
distinct [] = True
distinct (x:xs) = x `notElem` xs && distinct xs

islegal :: Placement -> Bool
islegal places = and [
  distinct (map fst places),
  distinct (map snd places),
  distinct (map (uncurry (+)) places),
  distinct (map (uncurry (-)) places)]

pretty :: Placement -> String
pretty places = unlines [[if (r, c) `elem` places then '♛' else '.'
                | c <- enumFromTo 0 (toEnum $ length places - 1)]
                | r <- enumFromTo 0 (toEnum $ length places - 1)]

mkcol :: Int -> Symbolic (Bit 8)
mkcol n = do
    x <- free_Bit
    assert (x < toEnum n)
    return x

bit_nqueens :: Int -> SMT ()
bit_nqueens n = do
  result <- query $ do
         let rows = enumFromTo 0 (toEnum n-1)
         cols <- sequence $ replicate n (mkcol n)
         let places = zip rows cols
         assert (islegal places)
         return places
  liftIO $ case result of
              Nothing -> putStrLn "no solution"
              Just v -> putStrLn (pretty v)

