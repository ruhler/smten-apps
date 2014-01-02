
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module BitCell (BitCell) where

import Smten.Prelude
import Smten.Symbolic
import Smten.Data.Bit
import Smten.Data.Function

import Cell

type BitCell = Bit 4

instance Cell BitCell where
    mkCell = toEnum
    deCell = fromEnum

    freeCell = do
        x <- free_Bit
        assert ((x > 0) && (x <= 9))
        return x

    distinctCell = distinct

