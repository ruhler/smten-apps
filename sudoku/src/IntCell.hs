
{-# LANGUAGE TypeSynonymInstances #-}
module IntCell (IntCell) where

import Smten.Prelude
import Smten.Control.Monad
import Cell

type IntCell = Int

instance Cell IntCell where
    mkCell = id
    deCell = id
    freeCell = msum (map return [1..9])
    distinctCell = distinct

