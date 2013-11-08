
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}
module Bits (
    Bit, Bits,
    andB, orB, notB, accessB, valB, updB, bulkupdB, intB, addB, subB,
    ltB, gtB, leB, geB, eqB, neqB,
    shlB, shrB, xor, xorB, castB, extractB,
    freeBits,
    ) where

import Smten.Prelude
import Smten.Symbolic

type Bit = Bool
type Bits = [Bit] -- head is the least significant bit

padto :: Bits -> Int -> [Bit]
padto bs w = take w (bs ++ repeat False)

andB :: Bits -> Bits -> Bits
andB = zipWith (&&)

orB :: Bits -> Bits -> Bits
orB = zipWith (||)

ltB :: Bits -> Bits -> Bit
ltB a b = isneg (a `subB` b)

leB :: Bits -> Bits -> Bit
leB a b = not (gtB a b)

gtB :: Bits -> Bits -> Bit
gtB a b = isneg (b `subB` a)

geB :: Bits -> Bits -> Bit
geB a b = not (ltB a b)

eqB :: Bits -> Bits -> Bit
eqB a b = and $ zipWith (==) a b

neqB :: Bits -> Bits -> Bit
neqB a b = not (eqB a b)

notB :: Bits -> Bits 
notB = map not

-- Right shift removes the least significant bits
shrB :: Bits -> Int -> Bits
shrB a b = drop b a ++ replicate b False

-- Left shift removes the most significant bits
shlB :: Bits -> Int -> Bits
shlB a b = take (length a) $ replicate b False ++ a

accessB :: Bits -> Int -> Bit
accessB bs i = bs !! i

-- freeBits w n
-- Construct a symbolic bit vector
--  w - the width of the vector
--  n - the number of free bits (the rest of the bits are 0)
freeBits :: Int -> Int -> Symbolic Bits
freeBits w n = do
  let n' = min n w
  vals <- sequence $ replicate n' free
  return $ vals ++ replicate (w - n') False

valB :: Bits -> Int
valB [] = 0
valB (True:xs) = 1 + 2 * (valB xs)
valB (False:xs) = 0 + 2 * (valB xs)

updB :: Bits -> Int -> Bit -> Bits
updB [] _ _ = error "updB: update out of bounds"
updB (x:xs) 0 v = v : xs
updB (x:xs) n v = x : updB xs (n-1) v

-- Do a bulk update of bits starting at the given index.
bulkupdB :: Bits -> Int -> Bits -> Bits
bulkupdB vals i vals' =
  let lo = take i vals
      mid = vals'
      hi = drop (i + length vals') vals
  in concat [lo, mid, hi]

-- Create a bit array from an Int
-- Least significant bit is first.
intB :: Int -> Int -> Bits
intB 0 _ = []
intB w v = case v `quotRem` 2 of
               (v2, b) -> (b == 1) : intB (w-1) v2

xor :: Bit -> Bit -> Bit
xor True True = False
xor True False = True
xor False True = True
xor False False = False

xorB :: Bits -> Bits -> Bits
xorB = zipWith xor

addB :: Bits -> Bits -> Bits
addB = add False

subB :: Bits -> Bits -> Bits
subB a b = add True a (map not b)

add :: Bit -> [Bit] -> [Bit] -> [Bit]
add _ [] [] = []
add c (a:as) (b:bs) = 
 let z = c `xor` a `xor` b
     c' = (c && a) || (c && b) || (a && b)
 in z : (add c' as bs)

isneg :: Bits -> Bit
isneg bs = last bs

-- | Cast a bit vector to the given width.
-- Truncates if the width is smaller than the bit vector width.
-- Appends 0 bits if the width is greater than the bit vector width.
castB :: Int -> Bits -> Bits
castB nw bs = take nw (bs ++ repeat False)

-- extractB bits lo hi
extractB :: Bits -> Int -> Int -> Bits
extractB bs lo hi = drop lo (take hi bs)

