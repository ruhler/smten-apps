
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module BitCell (BitCell) where

import Smten.Prelude
import Smten.Symbolic
import Smten.Data.Bit
import Smten.Data.Function

import Cell

type BitCell = Bit 9

c1 :: BitCell
c1 = 1

c2 :: BitCell
c2 = 2

c3 :: BitCell
c3 = 4

c4 :: BitCell
c4 = 8

c5 :: BitCell
c5 = 16

c6 :: BitCell
c6 = 32

c7 :: BitCell
c7 = 64

c8 :: BitCell
c8 = 128

c9 :: BitCell
c9 = 256

isValidCell :: BitCell -> Bool
isValidCell c = elem c [c1, c2, c3, c4, c5, c6, c7, c8, c9]

join :: [BitCell] -> Bit 9
join xs = foldl bv_or 0 xs

instance Cell BitCell where
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

