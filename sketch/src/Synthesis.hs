
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}
module Synthesis (
    synthesize,
    ) where

import Smten.Prelude
import Smten.Control.Monad
import qualified Smten.Data.Map as Map
import Smten.Data.Functor
import Smten.Symbolic
import Smten.Symbolic.SMT

import Bits
import Cegis
import Eval
import Input
import Generate
import Sketch

synthesize :: Prog -> SMT (Maybe Prog)
synthesize p = cegis (mkFreeProgramInput p) (generate p) [] evalP

