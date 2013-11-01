
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
  (fin, dbg) <- case args of 
                 [f, "-d", df] -> return (f, Just df)
                 [f] -> return (f, Nothing)
                 _ -> error "usage: sketch FILE [-d debug.dbg]"
  input <- readFile fin
  sk <- case evalStateT parseSketch input of
           Left msg -> fail msg
           Right x -> return x
  solver <- case dbg of
               Just fnm -> debug fnm yices2
               Nothing -> return yices2
  syn <- runSMT solver (synthesize (envof sk))
  case syn of
    Nothing -> fail "sketch not satisfiable"
    Just v -> putStrLn (pretty v)

