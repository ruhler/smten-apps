
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}
module Isolate0 (tests) where

import Smten.Prelude
import Smten.Data.Bit
import Smten.Smten.TypeLits
import Smten.Symbolic
import Smten.Symbolic.Solver.Yices2
import Smten.Symbolic.SMT
import Smten.Tests.Test

import S2QBF

-- Test the isolate0 sketch

-- The specification: returns a bit vector with a single bit set, and that bit
-- is the right most zero bit in the given word.
isolate0 :: (SingI n) => Bit n -> Bit n
isolate0 = isolate0' 1

-- Helper function for isolate0. Takes a target mask as input.
isolate0' :: (SingI n) => Bit n -> Bit n -> Bit n
isolate0' m n
  | m == 0 = 0
  | bv_and m n == 0 = m
  | otherwise = isolate0' (bv_shl m 1) n

-- isolate0 1010_0111 = 167
-- To get:  0000_1000 = 8
example :: Bit 8
example = isolate0 167

-- The sketch
data Holes a = Holes {
    hole1 :: a,
    hole2 :: a
}

instance (Eq a) => Eq (Holes a) where
    (==) (Holes a b) (Holes c d) = a == c && b == d

instance (Show a) => Show (Holes a) where
    show (Holes h1 h2) = concat [
        "Holes { hole1 = ", show h1, ", hole2 = ", show h2, " }"]

instance (Free a) => Free (Holes a) where
    free = do
       x <- free
       y <- free
       return (Holes x y)

isolate0sketch :: (SingI n) => Holes (Bit n) -> Bit n -> Bit n
isolate0sketch holes n
  = bv_and (bv_not (n + hole1 holes)) (n + hole2 holes)

holeswanted :: (SingI n) => Holes (Bit n)
holeswanted = Holes { hole1 = 0, hole2 = 1 }

iscorrect :: (SingI n) => Holes (Bit n) -> Bit n -> Bool
iscorrect h x = isolate0 x == isolate0sketch h x

qsketch :: SMT (Maybe (Holes (Bit 8)))
qsketch = s2qbf [] iscorrect

tests :: IO ()
tests = do
   r <- runSMT yices2 qsketch
   test "sketch" (r == Just holeswanted)

