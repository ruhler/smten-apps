
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module BitOneHotCell (BitOneHotCell) where

import Smten.Prelude
import Smten.Symbolic
import Smten.Data.Bit

import Cell

type BitOneHotCell = Bit 9

-- This enforces that no more than one bit is set.
-- The 'distinct' function and generated constraints will
-- enfoce that at least one bit is set.
isValidCell :: BitOneHotCell -> Bool
isValidCell c = c `bv_and` (c - 1) == 0

join :: [BitOneHotCell] -> Bit 9
join xs = foldl bv_or 0 xs

instance Cell BitOneHotCell where
    mkCell i = 1 `bv_shl` toEnum (i - 1)

    deCell 0x1 = 1
    deCell 0x2 = 2
    deCell 0x4 = 3
    deCell 0x8 = 4
    deCell 0x10 = 5
    deCell 0x20 = 6
    deCell 0x40 = 7
    deCell 0x80 = 8
    deCell 0x100 = 9
        
    freeCell = do
        x <- free_Bit
        assert (isValidCell x)
        return x

    distinctCell cells = join cells == 0x1FF

