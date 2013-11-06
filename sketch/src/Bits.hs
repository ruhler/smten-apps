
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}
module Bits (
    Bit, Bits,
    andB, orB, notB, accessB, valB, updB, bulkupdB, intB, addB, subB,
    ltB, gtB, leB, geB, eqB, neqB,
    shlB, shrB, xor, xorB, castB, extractB,
    mkbits, bits, width,
    freeBits,
    ) where

import Smten.Prelude
import Smten.Symbolic

type Bit = Bool

data Bits = Bits {
    width :: Int,   -- the length of bits
    bits :: [Bit]  -- head is the least significant bit
}

mkbits :: [Bit] -> Bits
mkbits l = Bits (length l) l

instance Eq Bits where
    (==) a b = bits a == bits b

instance Show Bits where
    show (Bits w b) = "Bits " ++ show b

-- TOOD: pad zeros to the lsbs or msbs?
padto :: Bits -> Int -> [Bit]
padto (Bits w bs) w' = bs ++ replicate (w'-w) False

andB :: Bits -> Bits -> Bits
andB a b = 
  let nw = max (width a) (width b)
      a' = padto a nw
      b' = padto b nw
  in Bits nw (zipWith (&&) a' b')

orB :: Bits -> Bits -> Bits
orB a b = 
  let nw = max (width a) (width b)
      a' = padto a nw
      b' = padto b nw
  in Bits nw (zipWith (||) a' b')

ltB :: Bits -> Bits -> Bit
ltB a b = isneg (a `subB` b)

leB :: Bits -> Bits -> Bit
leB a b = not (gtB a b)

gtB :: Bits -> Bits -> Bit
gtB a b = isneg (b `subB` a)

geB :: Bits -> Bits -> Bit
geB a b = not (ltB a b)

eqB :: Bits -> Bits -> Bit
eqB a b = and $ zipWith (==) (bits a) (bits b)

neqB :: Bits -> Bits -> Bit
neqB a b = not (eqB a b)

notB :: Bits -> Bits 
notB (Bits w bs) = Bits w (map not bs)

-- Right shift removes the least significant bits
shrB :: Bits -> Int -> Bits
shrB (Bits w a) b = Bits w (drop b a ++ replicate b False)

-- Left shift removes the most significant bits
shlB :: Bits -> Int -> Bits
shlB (Bits w a) b = Bits w (replicate b False ++ (take (w-b) a))

accessB :: Bits -> Int -> Bit
accessB (Bits w bs) i = bs !! i

-- freeBits w n
-- Construct a symbolic bit vector
--  w - the width of the vector
--  n - the number of free bits (the rest of the bits are 0)
freeBits :: Int -> Int -> Symbolic Bits
freeBits w n = do
  let n' = min n w
  vals <- sequence $ replicate n' free
  return $ Bits w (vals ++ replicate (w - n') False)

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

-- Do a bulk update of bits starting at the given index.
bulkupdB :: Bits -> Int -> Bits -> Bits
bulkupdB (Bits w vals) i (Bits w' vals') =
  let lo = take i vals
      mid = vals'
      hi = drop (i + length vals') vals
  in Bits w $ concat [lo, mid, hi]

-- Create a bit array from an Int
-- Least significant bit is first.
intB :: Int -> Int -> Bits
intB w x = 
  let f 0 _ = []
      f w v = case v `quotRem` 2 of
               (v2, b) -> (b == 1) : f (w-1) v2
  in Bits w (f w x)

xor :: Bit -> Bit -> Bit
xor True True = False
xor True False = True
xor False True = True
xor False False = False

xorB :: Bits -> Bits -> Bits
xorB a b = 
  let nw = max (width a) (width b)
      a' = padto a nw
      b' = padto b nw
  in Bits nw (zipWith xor a' b')

addB :: Bits -> Bits -> Bits
addB a b = 
  let nw = max (width a) (width b)
      a' = padto a nw
      b' = padto b nw
  in Bits nw (add False a' b')

subB :: Bits -> Bits -> Bits
subB a b = 
  let nw = max (width a) (width b)
      a' = padto a nw
      b' = padto b nw
  in Bits nw (add True a' (map not b'))

add :: Bit -> [Bit] -> [Bit] -> [Bit]
add _ [] [] = []
add c (a:as) (b:bs) = 
 let z = c `xor` a `xor` b
     c' = (c && a) || (c && b) || (a && b)
 in z : (add c' as bs)

isneg :: Bits -> Bit
isneg (Bits _ bs) = last bs

-- | Cast a bit vector to the given width.
-- Truncates if the width is smaller than the bit vector width.
-- Appends 0 bits if the width is greater than the bit vector width.
castB :: Int -> Bits -> Bits
castB nw b@(Bits w bs)
  | nw < w = Bits nw (take nw bs)
  | nw == w = b
  | otherwise = Bits nw (bs ++ replicate (nw - w) False)

-- extractB bits lo hi
extractB :: Bits -> Int -> Int -> Bits
extractB (Bits w bs) lo hi = Bits (hi-lo) (drop lo (take hi bs))

