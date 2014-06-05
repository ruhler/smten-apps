
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}
module Synthesis (
    synthesize,
    ) where

import Smten.Prelude
import Smten.Searches

import Cegis
import Eval
import Input
import Generate
import Options
import Program

synthesize :: Options -> Program -> Searches (Maybe Program)
synthesize opts p = cegis (mkFreeProgramInput opts p) (generate opts p) [] evalP

