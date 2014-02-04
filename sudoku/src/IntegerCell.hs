
{-# LANGUAGE TypeSynonymInstances #-}
module IntegerCell (IntegerCell) where

import Smten.Prelude
import Smten.Symbolic
import Cell

type IntegerCell = Integer

instance Cell IntegerCell where
    mkCell = toInteger
    deCell = fromInteger

    freeCell = do
        x <- free_Integer
        assert ((x > 0) && (x <= 9))
        return x

    distinctCell = distinct

