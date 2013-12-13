
import Smten.Prelude
import Smten.System.Environment
import Smten.System.Exit
import Smten.Symbolic
import Smten.Symbolic.SMT
import Smten.Symbolic.Solver.Smten
import Smten.Symbolic.Solver.STP
import Smten.Symbolic.Solver.Yices1
import Smten.Symbolic.Solver.Yices2
import Smten.Symbolic.Solver.MiniSat
import Smten.Symbolic.Solver.Debug
import Smten.Symbolic.Solver.Z3

import BoolNQueens
import BitNQueens
import IntNQueens
import IntegerNQueens



usage :: String
usage = "nqueens [-d debug] [-s yices1 | yices2 | stp | z3 | minisat] [-e Integer | Int | Bit | Bool] <n>"

lookuparg :: String -> [String] -> Maybe String
lookuparg k m = 
  case dropWhile ((/=) k) m of
     (_:x:_) -> Just x
     _ -> Nothing

lookupn :: [String] -> Maybe Int
lookupn [] = Nothing
lookupn (('-':_):_:xs) = lookupn xs
lookupn (n:_) = Just (read n)

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

  f <- case lookuparg "-e" args of
         Just "Integer" -> return integer_nqueens
         Just "Bool" -> return bool_nqueens
         Just "Bit" -> return bit_nqueens
         Just "Int" -> return int_nqueens
         Just x -> fail $ "Unknown elem type: " ++ x ++ ".\n" ++ usage
         Nothing -> return int_nqueens

  n <- case lookupn args of
          Nothing -> fail $ "no board size input\n" ++ usage 
          Just v -> return v

  runSMT solver (f n)

