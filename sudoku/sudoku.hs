
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}
import Smten.Prelude
import Smten.System.Environment
import Smten.System.Exit

import Smten.Symbolic
import Smten.Symbolic.Solver.Debug
import Smten.Symbolic.Solver.Yices1
import Smten.Symbolic.Solver.Yices2
import Smten.Symbolic.Solver.STP
import Smten.Symbolic.Solver.MiniSat
import Smten.Symbolic.Solver.Z3

import Board
import Cell
import IntegerCell
import BitCell
import EnumCell

-- solve_xx c slv board
--  c - a sample cell to specify the internal cell type to use
--  slv - the solver to use
--  board - the sudoku board
solve_xx :: forall c. (Cell c) => c -> Solver -> String -> IO ()
solve_xx _ slv board = do
    result <- run_symbolic slv $ do
       b <- readBoard board :: Symbolic (Board c)
       assert (isvalid b)
       return b
    case result of
        Nothing -> putStrLn "no solution"
        Just b' -> putStrLn $ printBoard b'

solve_ic :: Solver -> String -> IO ()
solve_ic = solve_xx (undefined :: IntegerCell)

solve_bc :: Solver -> String -> IO ()
solve_bc = solve_xx (undefined :: BitCell)

solve_ec :: Solver -> String -> IO ()
solve_ec = solve_xx (undefined :: EnumCell)

data EType = E_Integer | E_Bit | E_Enum

instance Eq EType where
    (==) E_Integer E_Integer = True
    (==) E_Bit E_Bit = True
    (==) E_Enum E_Enum = True
    (==) _ _ = False

usage :: String
usage = "sudoku [-d debug] [-s yices1 | yices2 | stp | z3] [-e Integer | Bit | Enum] [n]"

lookupn :: [String] -> Maybe Int
lookupn [] = Nothing
lookupn (('-':_):_:xs) = lookupn xs
lookupn (n:_) = Just (read n)


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

  elemtype <- case lookuparg "-e" args of
               Just "Integer" -> return E_Integer
               Just "Bit" -> return E_Bit
               Just "Enum" -> return E_Enum
               Just x -> fail $ "Unknown elem type: " ++ x ++ ".\n" ++ usage
               Nothing -> return E_Bit

  boardlist <- getContents

  let boards = case lookupn args of
                 Nothing -> lines boardlist
                 Just n -> take n (lines boardlist)

  let runsolve board = case elemtype of
          E_Integer -> solve_ic solver board
          E_Bit -> solve_bc solver board 
          E_Enum -> solve_ec solver board
  mapM_ runsolve boards

