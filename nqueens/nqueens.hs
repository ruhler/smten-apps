
import Smten.Prelude
import Smten.Control.Monad
import Smten.Symbolic
import Smten.Symbolic.Solver.Smten
import Smten.Symbolic.Solver.STP
import Smten.Symbolic.Solver.Yices1
import Smten.Symbolic.Solver.Yices2
import Smten.Symbolic.Solver.MiniSat
import Smten.Symbolic.Solver.Debug
import Smten.Symbolic.Solver.Z3
import Smten.System.Environment


-- Placement: A list of locations (row, col) for each queen.
-- Indices go from 0 to n-1
type Placement = [(Int, Int)]

distinct :: (Eq a) => [a] -> Bool
distinct [] = True
distinct (x:xs) = x `notElem` xs && distinct xs

issolution :: Placement -> Bool
issolution places = and [
  distinct (map fst places),
  distinct (map snd places),
  distinct (map (uncurry (+)) places),
  distinct (map (uncurry (-)) places)]

pretty :: Placement -> String
pretty places = unlines [[if (r, c) `elem` places then 'X' else '.'
                | c <- [0..(length places - 1)]]
                | r <- [0..(length places - 1)]]

nqueens :: Int -> IO ()
nqueens n = do
  putStrLn $ "nqueens " ++ show n ++ "..."
  result <- run_symbolic smten $ do
         let rows = [0..(n-1)]
         cols <- sequence $ replicate n (msum (map return [0..(n-1)]))
         let places = zip rows cols
         assert (issolution places)
         return places
  case result of
    Nothing -> putStrLn "no solution"
    Just v -> putStrLn (pretty v)

usage :: IO ()
usage = putStrLn "nqueens <n>"

main :: IO ()
main = do
  args <- getArgs
  case args of
     [x] -> nqueens (read x)
     _ -> usage

