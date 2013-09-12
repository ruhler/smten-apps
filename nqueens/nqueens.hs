
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}

import Smten.Prelude
import Smten.Data.Array
import Smten.Symbolic
import Smten.Symbolic.Solver.Yices2
import Smten.System.Environment


-- Placement: for each column, list the row placement.
-- Indices go from 0 to n-1
type Placement = Array Int Integer

issolution :: Placement -> Bool
issolution x =
  let nm1 = snd (bounds x)
      pairs = [(i, j) | i <- [0..nm1-1], j <- [i+1 .. nm1]] :: [(Int, Int)]
      valid (i, j) = and [
            (x ! i) /= (x ! j),
            (toEnum (j - i)) /= ((x ! j) - (x ! i)),
            (toEnum (i - j)) /= ((x ! j) - (x ! i))]
  in all valid pairs

pretty :: [Integer] -> String
pretty xs = 
  let row i = replicate i '.' ++ "X" ++ replicate (length xs - 1 - i) '.'
  in unlines (map row (map fromInteger xs))

nqueens :: Int -> IO ()
nqueens n = do
  putStrLn $ "nqueens " ++ show n ++ "..."
  r <- run_symbolic yices2 $ do
         let freer = do
                x <- free
                assert (x >= 0 && x < (toEnum n))
                return x
         rs <- sequence (replicate n freer)
         let place = listArray (0, n-1) rs
         assert (issolution place)
         return rs
  case r of
    Nothing -> putStrLn "no solution"
    Just v -> putStrLn (pretty v)

usage :: IO ()
usage = putStrLn "nqueens <n>"

read_int' :: Int -> String -> Int
read_int' x ('0':xs) = read_int' (x*10 + 0) xs
read_int' x ('1':xs) = read_int' (x*10 + 1) xs
read_int' x ('2':xs) = read_int' (x*10 + 2) xs
read_int' x ('3':xs) = read_int' (x*10 + 3) xs
read_int' x ('4':xs) = read_int' (x*10 + 4) xs
read_int' x ('5':xs) = read_int' (x*10 + 5) xs
read_int' x ('6':xs) = read_int' (x*10 + 6) xs
read_int' x ('7':xs) = read_int' (x*10 + 7) xs
read_int' x ('8':xs) = read_int' (x*10 + 8) xs
read_int' x ('9':xs) = read_int' (x*10 + 9) xs
read_int' x _ = x

read_int :: String -> Int
read_int ('-':xs) = negate (read_int xs)
read_int xs = read_int' 0 xs

main :: IO ()
main = do
  args <- getArgs
  case args of
     [x] -> nqueens (read_int x)
     _ -> usage

