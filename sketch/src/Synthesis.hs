
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}
module Synthesis (
    synthesize,
    ) where

import Smten.Prelude
import Smten.Symbolic.SMT

import Cegis
import Eval
import Input
import Generate
import Options
import Program

synthesize :: Options -> Program -> SMT (Maybe Program)
synthesize opts p = cegis (mkFreeProgramInput opts p) (generate opts p) [] evalP

