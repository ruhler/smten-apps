
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}

import Smten.Prelude
import Smten.Control.Monad.State
import Smten.Symbolic.SMT
import Smten.Symbolic.Solver.Debug
import Smten.Symbolic.Solver.Yices2

import Grammar
import Ppr
import Typing
import Synthesis

main :: IO ()
main = do
  input <- getContents
  sk <- case evalStateT parseSketch input of
            Left msg -> fail msg
            Right x -> return x
  let sktyped = tinferP sk
  sksynthed <- runSMT yices2 (synthesize sktyped)
  putStrLn (pretty sksynthed)

