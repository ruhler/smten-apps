
import Smten.Prelude
import Smten.Data.Array
import Smten.Symbolic.SMT
import Smten.Symbolic.Solver.Debug
import Smten.Symbolic.Solver.MiniSat
import Smten.Symbolic.Solver.STP
import Smten.Symbolic.Solver.Z3
import Smten.Symbolic.Solver.Yices1
import Smten.Symbolic.Solver.Yices2
import Smten.System.Environment
import Smten.System.Exit
import Aiger
import AigerPCheck

usage :: String
usage = unlines $ [
    "aiger [-d debug] [-k0 n] [-ki n] [-s yices1 | yices2 | stp | z3 | minisat] < FILE",
    "  -k0 n    Start with initial bound n",
    "  -ki n    Use increments of n for the bound"
    ]

lookuparg :: String -> [String] -> Maybe String
lookuparg k m = 
  case dropWhile ((/=) k) m of
     (_:x:_) -> Just x
     _ -> Nothing

main :: IO ()
main = do
  args <- getArgs
  if "--help" `elem` args
     then putStrLn usage >> exitSuccess
     else return ()

  basesolver <- case lookuparg "-s" args of
                   Just "yices1" -> return yices1
                   Just "yices2" -> return yices2
                   Just "stp" -> return stp
                   Just "z3" -> return z3
                   Just "minisat" -> return minisat
                   Just x -> fail $ "Unknown solver: " ++ x ++ ".\n" ++ usage
                   Nothing -> return yices2

  solver <- case lookuparg "-d" args of
                Just fn -> debug fn basesolver
                Nothing -> return basesolver

  k0 <- case lookuparg "-k0" args of
          Just n -> return (read n)
          Nothing -> return 0

  ki <- case lookuparg "-ki" args of
          Just n -> return (read n)
          Nothing -> return 1

  txt <- getContents
  let aig = readAsciiAiger txt

  -- Note: we only look for the first output.
  -- Presumably we need to check for them all?
  result <- runSMT solver $ aigercheck k0 ki aig (aig_outputs aig ! 1)
  case result of
    Nothing -> putStrLn "0\n."
    Just v -> do
       putStrLn "1"
       putStrLn "b0"
       putStrLn $ showVector (aig_reset aig)
       putStr . unlines $ map showVector v
       putStrLn "."

