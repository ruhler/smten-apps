
-- | Implementations of Symbolic Characters.
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}
module SChar where

import Smten.Prelude
import Smten.Data.Char
import Smten.Data.Function
import Smten.Data.Functor

import Smten.Symbolic
import Smten.Data.Bit

class (Ord a) => SChar a where
    toSChar :: Char -> a
    fromSChar :: a -> Char
    freeSChar :: Symbolic a

toSCharString :: (SChar c) => String -> [c]
toSCharString = map toSChar

fromSCharString :: (SChar c) => [c] -> String
fromSCharString = map fromSChar

-- Create a free element string of fixed length.
freeSCharString :: (SChar c) => Int -> Symbolic [c]
freeSCharString x = sequence (replicate x freeSChar)

instance SChar Integer where
    toSChar = toEnum . ord
    fromSChar = chr . fromInteger
    freeSChar = do
        x <- free
        assert (x >= 0 && x < 256)
        return x

-- A sample integer character
integerSChar :: Integer
integerSChar = 0

data BitSChar = BitSChar { bschar :: Bit 8 }

instance Eq BitSChar where
    (==) = (==) `on` bschar

instance Ord BitSChar where
    (<) (BitSChar a) (BitSChar b) = a < b
    (>) (BitSChar a) (BitSChar b) = a > b
    (<=) (BitSChar a) (BitSChar b) = a <= b
    (>=) (BitSChar a) (BitSChar b) = a >= b
    compare (BitSChar a) (BitSChar b) = compare a b

instance SChar BitSChar where
    toSChar = BitSChar . fromInteger . toEnum . ord
    fromSChar (BitSChar x) = chr . fromInteger . bv_value $ x
    freeSChar = BitSChar <$> free

bitSChar :: BitSChar
bitSChar = BitSChar 0

