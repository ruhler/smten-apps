
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}

import Smten.Prelude
import Smten.Control.Monad.State
import Smten.Symbolic.SMT
import Smten.Symbolic.Solver.Debug
import Smten.Symbolic.Solver.Yices2
import Smten.System.Environment
import Smten.System.IO

import Grammar
import Ppr
import Sketch
import Synthesis

main :: IO ()
main = do
  args <- getArgs
  input <- case args of 
             [f] -> readFile f
             _ -> error "usage: sketch FILE"
  sk <- case evalStateT parseSketch input of
            Left msg -> fail msg
            Right x -> return x
  syn <- runSMT yices2 (synthesize (envof sk))
  case syn of
    Nothing -> fail "sketch not satisfiable"
    Just v -> putStrLn (pretty v)

