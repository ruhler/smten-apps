
import Smten.Prelude
import Smten.Control.Monad.State
import Smten.Symbolic
import Smten.Symbolic.SMT
import Smten.Symbolic.Solver.Debug
import Smten.Symbolic.Solver.Yices1
import Smten.Symbolic.Solver.Yices2
import Smten.Symbolic.Solver.STP
import Smten.Symbolic.Solver.Z3
import Smten.Symbolic.Solver.MiniSat
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
    "  sketch [OPTIONS] [FILE...]",
    "",
    " Options:",
    "    -d FILE            Output debug info to the given file",
    "    -s solver          Select the back-end solver to use:",
    "                         yices1, yices2, z3, stp, or minisat",
    "    --bnd-cbits N      Set the bnd-cbits value to N",
    "    --bnd-inbits N     Set the bnd-inbits value to N",
    ""]

data CmdArgs = CmdArgs {
    cmd_err :: Maybe String,
    cmd_files :: [String],
    cmd_debug :: Maybe String,
    cmd_solver :: Solver,
    cmd_opts :: Options
}

defargs :: CmdArgs
defargs = CmdArgs {
    cmd_err = Nothing,
    cmd_files = [],
    cmd_debug = Nothing,
    cmd_solver = yices2,
    cmd_opts = defaultOptions
}

-- Parse the command line arguments, using the given defaults
parseargs :: [String] -> CmdArgs -> CmdArgs
parseargs s defs =
  case s of
    [] -> defs
    ("-d" : dbg : rest) -> (parseargs rest defs) { cmd_debug = Just dbg }
    ("-s" : "yices1" : rest) -> (parseargs rest defs) { cmd_solver = yices1 }
    ("-s" : "yices2" : rest) -> (parseargs rest defs) { cmd_solver = yices2 }
    ("-s" : "stp" : rest) -> (parseargs rest defs) { cmd_solver = stp }
    ("-s" : "z3" : rest) -> (parseargs rest defs) { cmd_solver = z3 }
    ("-s" : "minisat" : rest) -> (parseargs rest defs) { cmd_solver = minisat }
    ("-s" : slv : _) -> defs { cmd_err = Just ("unrecognized solver: " ++ slv) }
    ("--bnd-cbits" : n : rest) ->
        let args = parseargs rest defs
            opts' = (cmd_opts args) { bnd_cbits = read n }
        in args { cmd_opts = opts' }
    ("--bnd-inbits" : n : rest) ->
        let args = parseargs rest defs
            opts' = (cmd_opts args) { bnd_inbits = read n }
        in args { cmd_opts = opts' }
    (('-':f) : _) -> defs { cmd_err = Just ("unrecognized flag: " ++ f) }
    (f : rest) ->
       let x = parseargs rest defs
       in x { cmd_files = f : (cmd_files x) }

main :: IO ()
main = do
  argv <- getArgs
  let args = parseargs argv defargs
  case cmd_err args of
     Just err -> error $ unlines [err, usage]
     Nothing -> return ()

  solver <- case cmd_debug args of
               Just fnm -> debug fnm (cmd_solver args)
               Nothing -> return (cmd_solver args)

  flip mapM_ (cmd_files args) $ \fin -> do
    putStrLn $ "Running sketch on " ++ show fin ++ "..."
    input <- readFile fin
    sk <- case evalStateT parseSketch input of
             Left msg -> fail msg
             Right x -> return x
    --putStrLn $ "POST PARSE: " ++ show sk

    let st = static (envof sk)
    --putStrLn $ "POST STATIC: " ++ show st

    syn <- runSMT solver (synthesize (cmd_opts args) (envof st))
    case syn of
      Nothing -> fail "sketch not satisfiable"
      Just v -> putStrLn (pretty v)

