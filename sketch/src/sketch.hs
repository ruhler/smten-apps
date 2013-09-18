
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}

import Smten.Prelude
import Smten.Control.Monad.State
import Smten.Symbolic.SMT
import Smten.Symbolic.Solver.Yices2

import Grammar
import Ppr
import Synthesis

main :: IO ()
main = do
  input <- getContents
  sk <- case evalStateT parseSketch input of
            Left msg -> fail msg
            Right x -> return x
  sk' <- runSMT yices2 (synthesize sk)
  putStrLn (pretty sk')

