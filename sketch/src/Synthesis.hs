
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

synthesize :: Options -> ProgEnv -> SMT (Maybe Prog)
synthesize opts p = cegis (mkFreeProgramInput opts (declsof p)) (generate opts p) [] evalP

