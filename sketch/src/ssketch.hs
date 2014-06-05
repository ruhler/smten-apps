
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
import Options
import Ppr
import Program
import Static
import Synthesis

usage :: String
usage = unlines [
    "Usage:",
    "  ssketch [OPTIONS] [FILE...]",
    "",
    " Options:",
    "    -d FILE                Output debug info to the given file",
    "    -s solver              Select the back-end solver to use:",
    "                            yices1, yices2, z3, stp, or minisat",
    "    --bnd-cbits N          Set the bnd-cbits value to N",
    "    --bnd-inbits N         Set the bnd-inbits value to N",
    "    --bnd-unroll-amnt N    Set the bnd-unroll-amnt value to N",
    "    --bnd-inline-amnt N    Set the bnd-inline-amnt value to N",
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

parseargs :: [String] -> CmdArgs
parseargs s =
  case s of
    [] -> defargs
    ("-d" : dbg : rest) -> (parseargs rest) { cmd_debug = Just dbg }
    ("-s" : "yices1" : rest) -> (parseargs rest) { cmd_solver = yices1 }
    ("-s" : "yices2" : rest) -> (parseargs rest) { cmd_solver = yices2 }
    ("-s" : "stp" : rest) -> (parseargs rest) { cmd_solver = stp }
    ("-s" : "z3" : rest) -> (parseargs rest) { cmd_solver = z3 }
    ("-s" : "minisat" : rest) -> (parseargs rest) { cmd_solver = minisat }
    ("-s" : slv : _) -> defargs { cmd_err = Just ("unrecognized solver: " ++ slv) }
    ("--bnd-cbits" : n : rest) ->
        let args = parseargs rest
            opts' = (cmd_opts args) { bnd_cbits = read n }
        in args { cmd_opts = opts' }
    ("--bnd-inbits" : n : rest) ->
        let args = parseargs rest
            opts' = (cmd_opts args) { bnd_inbits = read n }
        in args { cmd_opts = opts' }
    ("--bnd-unroll-amnt" : n : rest) ->
        let args = parseargs rest
            opts' = (cmd_opts args) { bnd_unroll_amnt = read n }
        in args { cmd_opts = opts' }
    ("--bnd-inline-amnt" : n : rest) ->
        let args = parseargs rest
            opts' = (cmd_opts args) { bnd_inline_amnt = read n }
        in args { cmd_opts = opts' }
    (('-':f) : _) -> defargs { cmd_err = Just ("unrecognized flag: " ++ f) }
    (f : rest) ->
       let x = parseargs rest
       in x { cmd_files = f : (cmd_files x) }

main :: IO ()
main = do
  -- Parse command line arguments
  argv <- getArgs
  let args = parseargs argv 
  case cmd_err args of
     Just err -> error $ unlines [err, usage]
     Nothing -> return ()

  -- Setup the back-end solver
  solver <- case cmd_debug args of
               Just fnm -> debug fnm (cmd_solver args)
               Nothing -> return (cmd_solver args)

  -- Run sketch on each input file
  flip mapM_ (cmd_files args) $ \fin -> do
    putStrLn $ "Running ssketch on " ++ show fin ++ "..."

    -- Parse the input file
    input <- readFile fin
    (sk, skopts) <- case evalStateT parseSketch input of
             Left msg -> fail msg
             Right (ds, opts) -> return (program ds, opts)

    -- Update the options based on any pragmas in the file
    let args' = parseargs (argv ++ words skopts)
        opts = cmd_opts args'
    case cmd_err args' of
       Just err -> putStrLn $ unlines ["warning: " ++ err, "(from use of a pragma)"]
       Nothing -> return ()
    putStrLn $ "Using Options: " ++ show opts
    putStrLn $ "Parsed: " ++ show sk
      

    -- Perform static analysis and execution
    let st = static sk
    putStrLn $ "Statically Evaluated: " ++ show st

    -- Run the synthesizer
    syn <- runSMT solver (synthesize opts st)
    case syn of
      Nothing -> fail "sketch not satisfiable"
      Just v -> putStrLn (pretty v)

