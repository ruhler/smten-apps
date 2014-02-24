
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module BitCell (BitCell) where

import Smten.Prelude
import Smten.Symbolic
import Smten.Data.Bit

import Cell

type BitCell = Bit 4

instance Cell BitCell where
    mkCell c = toEnum (c-1)
    deCell c = fromEnum c + 1

    freeCell = do
        x <- free_Bit
        assert (x <= 8)
        return x

    distinctCell = distinct

