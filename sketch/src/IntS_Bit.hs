
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- Implementation of IntS based on Smten Bit type.
module IntS_Bit (
    IntS, fromInt, freeInt,
  ) where

import Smten.Prelude
import Smten.Control.Monad
import Smten.Data.Bit
import Smten.Symbolic

newtype IntS = IntS (Bit 16)
    deriving (Eq, Ord, Num, Enum, Integral, Real)

instance Show IntS where
    show (IntS x) = show (fromEnum x)

fromInt :: Int -> IntS
fromInt = IntS . toEnum

-- Compute 2^n
exp2 :: Int -> Int
exp2 0 = 1
exp2 n = 2 * exp2 (n-1)

-- Make a set of ints from 0 to 2^n-1
freeInt :: Int -> Symbolic IntS
freeInt n = msum (map (return . fromInt) [0..(exp2 n - 1)])

