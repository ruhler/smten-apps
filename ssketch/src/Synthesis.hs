
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

synthesize :: Bool -> Options -> Program -> Searches (Maybe Program)
synthesize verbose opts p = cegis verbose (mkFreeProgramInput opts p) (generate opts p) [] evalP

