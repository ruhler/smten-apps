
import Smten.Prelude
import Smten.System.Environment
import Smten.System.Exit
import Smten.System.IO0 (linebuffer)
import Smten.Search
import Smten.Search.Solver.STP
import Smten.Search.Solver.Yices1
import Smten.Search.Solver.Yices2
import Smten.Search.Solver.MiniSat
import Smten.Search.Solver.Debug
import Smten.Search.Solver.Z3

import BoolNQueens
import Bool2NQueens
import BitNQueens
import IntNQueens
import IntegerNQueens



usage :: String
usage = "nqueens [-d debug] [-s yices1 | yices2 | stp | z3 | minisat] [-e Integer | Int | Bit | Bool | Bool2] <n>"

lookuparg :: String -> [String] -> Maybe String
lookuparg k m = 
  case dropWhile ((/=) k) m of
     (_:x:_) -> Just x
     _ -> Nothing

lookupn :: [String] -> Maybe Int
lookupn [] = Nothing
lookupn (('-':_):_:xs) = lookupn xs
lookupn (n:_) = Just (read n)

-- Given the placement as a list of column positions for each queen
-- in row order, print a pretty result.
-- The columns are 0-indexed
pretty :: Int -> [Int] -> String
pretty n xs =
  let mkrow :: Int -> String
      mkrow i = replicate i '.' ++ ['♛'] ++ replicate (n - i - 1) '.'
  in unlines (map mkrow xs)

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
         Just "Bool2" -> return bool2_nqueens
         Just "Bit" -> return bit_nqueens
         Just "Int" -> return int_nqueens
         Just x -> fail $ "Unknown elem type: " ++ x ++ ".\n" ++ usage
         Nothing -> return int_nqueens

  n <- case lookupn args of
          Nothing -> fail $ "no board size input\n" ++ usage 
          Just v -> return v

  -- If n is less than zero, we try all n starting from 0 to (-n) and print a
  -- single line for each solution.
  if (n < 0)
    then do
        linebuffer
        flip mapM_ [0..(negate n)] $ \n -> do
            search solver (f n) >>= print
    else do
      mxs <- search solver (f n)
      case mxs of
        Nothing -> putStrLn "no solution"
        Just xs -> putStrLn $ pretty n xs

