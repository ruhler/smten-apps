
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- Implementation of IntS based on Smten Int type.
module IntS_Int (
    IntS, fromInt, freeInt,
  ) where

import Smten.Prelude
import Smten.Control.Monad
import Smten.Search

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
freeInt :: Int -> Space IntS
freeInt n = msum (map (return . fromInt) [0..(exp2 n - 1)])

