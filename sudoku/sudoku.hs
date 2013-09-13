
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}
import Smten.Prelude
import Smten.System.Environment

import Smten.Symbolic
import Smten.Symbolic.Solver.Yices2

import Board
import IntegerCell
import BitCell
import EnumCell

solve :: String -> IO ()
solve board = do
    result <- run_symbolic yices2 $ do
       b <- readBoard board :: Symbolic (Board IntegerCell)
       assert (isvalid b)
       return b
    case result of
        Nothing -> putStrLn "no solution"
        Just b' -> putStrLn $ printBoard b'

main :: IO ()
main = do
  boards <- getContents
  mapM_ solve (lines boards)
  

