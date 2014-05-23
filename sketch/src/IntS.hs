
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module IntS (
    IntS, fromInt, freeInt,
    shlI, shrI,
  ) where

import Smten.Prelude
import Smten.Control.Monad
import Smten.Symbolic
import GHC.Real

newtype IntS = IntS Int
    deriving (Eq, Enum, Integral, Real, Ord, Num)

instance Show IntS where
    show (IntS x) = show x

fromInt :: Int -> IntS
fromInt = IntS

-- Compute 2^n
exp2 :: Int -> Int
exp2 0 = 1
exp2 n = 2 * exp2 (n-1)

-- Make a set of ints from 0 to 2^n-1
freeInt :: Int -> Symbolic IntS
freeInt n = msum (map (return . fromInt) [0..(exp2 n - 1)])

shlI :: IntS -> IntS -> IntS
shlI x n
 | n < 0 = 0
 | n == 0 = x
 | otherwise = shlI (2*x) (n-1)

shrI :: IntS -> IntS -> IntS
shrI x n
 | n < 0 = 0
 | n == 0 = x
 | otherwise = shrI (x `quot` 2) (n-1)

