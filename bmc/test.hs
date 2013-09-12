
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}

import Smten.Prelude
import Smten.Symbolic
import Smten.Symbolic.Solver.Pure

import BMC
import ShiftReg


-- TODO: Have this fail if it gives the wrong answer.
main :: IO ()
main = do
   s <- run_symbolic pure (check shiftregm shiftregf 3)
   putStrLn $ show s

