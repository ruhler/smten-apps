
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
import Sketch

synthesize :: ProgEnv -> SMT (Maybe Prog)
synthesize p = cegis (mkFreeProgramInput p) (generate p) [] evalP

