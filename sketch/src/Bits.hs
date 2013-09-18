
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}
module Bits (
    Bit,
    Bits, andB, orB, accessB,
    freeBits,
    ) where

import Smten.Prelude
import Smten.Symbolic

type Bit = Bool

data Bits = Bits {
    width :: Int,   -- the length of bits
    bits :: [Bit]  -- head is the most significant bit
}

instance Eq Bits where
    (==) a b = bits a == bits b

padto :: Bits -> Int -> Bits
padto (Bits w bs) w' = Bits w' (replicate (w'-w) False ++ bs)

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
accessB (Bits w bs) i = bs !! (w - i - 1)

freeBits :: Int -> Symbolic Bits
freeBits n = do
  vals <- sequence $ replicate n free
  return (Bits n vals)

