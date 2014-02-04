
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module BitOneHotCell (BitOneHotCell) where

import Smten.Prelude
import Smten.Symbolic
import Smten.Data.Bit

import Cell

type BitOneHotCell = Bit 9

c1 :: BitOneHotCell
c1 = 1

c2 :: BitOneHotCell
c2 = 2

c3 :: BitOneHotCell
c3 = 4

c4 :: BitOneHotCell
c4 = 8

c5 :: BitOneHotCell
c5 = 16

c6 :: BitOneHotCell
c6 = 32

c7 :: BitOneHotCell
c7 = 64

c8 :: BitOneHotCell
c8 = 128

c9 :: BitOneHotCell
c9 = 256

isValidCell :: BitOneHotCell -> Bool
isValidCell c = elem c [c1, c2, c3, c4, c5, c6, c7, c8, c9]

join :: [BitOneHotCell] -> Bit 9
join xs = foldl bv_or 0 xs

instance Cell BitOneHotCell where
    mkCell i = [c1, c2, c3, c4, c5, c6, c7, c8, c9] !! (i-1)

    deCell c = 
           if c == c1 then 1
      else if c == c2 then 2
      else if c == c3 then 3
      else if c == c4 then 4
      else if c == c5 then 5
      else if c == c6 then 6
      else if c == c7 then 7
      else if c == c8 then 8
      else 9

    freeCell = do
        x <- free_Bit
        assert (isValidCell x)
        return x

    distinctCell cells = join cells == 0x1FF

