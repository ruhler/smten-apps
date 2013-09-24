
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}
module Bits (
    Bit,
    Bits, andB, orB, accessB, valB, updB, intB,
    freeBits,
    ) where

import Smten.Prelude
import Smten.Symbolic

type Bit = Bool

data Bits = Bits {
    width :: Int,   -- the length of bits
    bits :: [Bit]  -- head is the least significant bit
}

instance Eq Bits where
    (==) a b = bits a == bits b

instance Show Bits where
    show (Bits w b) = "Bits " ++ show b

-- TOOD: pad zeros to the lsbs or msbs?
padto :: Bits -> Int -> Bits
padto (Bits w bs) w' = Bits w' (bs ++ replicate (w'-w) False)

andB :: Bits -> Bits -> Bits
andB a b = 
  let nw = max (width a) (width b)
      a' = padto a nw
      b' = padto b nw
  in Bits nw (zipWith (&&) (bits a') (bits b'))

orB :: Bits -> Bits -> Bits
orB a b = 
  let nw = max (width a) (width b)
      a' = padto a nw
      b' = padto b nw
  in Bits nw (zipWith (||) (bits a') (bits b'))

accessB :: Bits -> Int -> Bit
accessB (Bits w bs) i = bs !! i

freeBits :: Int -> Symbolic Bits
freeBits n = do
  vals <- sequence $ replicate n free
  return (Bits n vals)

valB :: Bits -> Int
valB (Bits _ vals) = 
  let f :: [Bit] -> Int
      f [] = 0
      f (True:xs) = 1 + 2 * (f xs)
      f (False:xs) = 0 + 2 * (f xs)
  in f vals

updB :: Bits -> Int -> Bit -> Bits
updB (Bits w vals) i v =
  let f _ [] = error "updB: update out of bounds"
      f 0 (x:xs) = v:xs
      f n (x:xs) = x : f (n-1) xs
  in Bits w (f i vals)

-- Create a bit array from an Int
-- Least significant bit is first.
intB :: Int -> Int -> Bits
intB w x = 
  let f 0 _ = []
      f w v = case w `quotRem` 2 of
               (w2, b) -> (b == 1) : f (w-1) w2
  in Bits w (f w x)

