
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
import Static
import Synthesis

usage :: String
usage = unlines [
    "Usage:",
    "  sketch FILE [OPTIONS]",
    "",
    " Options:",
    "    -d FILE            Output debug info to the given file",
    "    --bnd-cbits N      Set the bnd-cbits value to N",
    "    --bnd-inbits N     Set the bnd-inbits value to N",
    ""]

data CmdArgs = CmdArgs {
    cmd_err :: Maybe String,
    cmd_file :: Maybe String,
    cmd_debug :: Maybe String,
    cmd_opts :: Options
}

defargs :: CmdArgs
defargs = CmdArgs {
    cmd_err = Nothing,
    cmd_file = Nothing,
    cmd_debug = Nothing,
    cmd_opts = defaultOptions
}

parseargs :: [String] -> CmdArgs
parseargs s =
  case s of
    [] -> defargs
    ("-d" : dbg : rest) -> (parseargs rest) { cmd_debug = Just dbg }
    ("--bnd-cbits" : n : rest) ->
        let args = parseargs rest
            opts' = (cmd_opts args) { bnd_cbits = read n }
        in args { cmd_opts = opts' }
    ("--bnd-inbits" : n : rest) ->
        let args = parseargs rest
            opts' = (cmd_opts args) { bnd_inbits = read n }
        in args { cmd_opts = opts' }
    (('-':f) : _) -> defargs { cmd_err = Just ("unrecognized flag: " ++ f) }
    (f : rest) -> (parseargs rest) { cmd_file = Just f }

main :: IO ()
main = do
  argv <- getArgs
  let args = parseargs argv
  case cmd_err args of
     Just err -> error $ unlines [err, usage]
     Nothing -> return ()

  input <- case cmd_file args of
              Just fin -> readFile fin
              Nothing -> error $ unlines ["no input file", usage]
    
  sk <- case evalStateT parseSketch input of
           Left msg -> fail msg
           Right x -> return x
  --putStrLn $ "POST PARSE: " ++ show sk

  let st = static (envof sk)
  --putStrLn $ "POST STATIC: " ++ show st

  solver <- case cmd_debug args of
               Just fnm -> debug fnm yices2
               Nothing -> return yices2

  syn <- runSMT solver (synthesize (cmd_opts args) (envof st))
  case syn of
    Nothing -> fail "sketch not satisfiable"
    Just v -> putStrLn (pretty v)

