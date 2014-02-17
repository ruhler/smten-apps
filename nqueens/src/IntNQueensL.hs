

-- | Implementation of nqueens based on the Int type.
module IntNQueensL ( int_nqueensl ) where

import Control.Monad

-- Placement: A list of locations (row, col) for each queen.
-- Indices go from 0 to n-1
type Placement = [(Int, Int)]

distinct :: (Eq a) => [a] -> Bool
distinct [] = True
distinct (x:xs) = x `notElem` xs && distinct xs

islegal :: Placement -> Bool
islegal places = and [
  distinct (map fst places),
  distinct (map snd places),
  distinct (map (uncurry (+)) places),
  distinct (map (uncurry (-)) places)]

pretty :: Placement -> String
pretty places = unlines [[if (r, c) `elem` places then '♛' else '.'
                | c <- [0..(length places - 1)]]
                | r <- [0..(length places - 1)]]

mkcol :: Int -> [Int]
mkcol n = msum (map return [0..(n-1)])

int_nqueensl :: Int -> IO ()
int_nqueensl n = do
  let result = do
         let rows = [0..(n-1)]
         cols <- sequence $ replicate n (mkcol n)
         let places = zip rows cols
         guard (islegal places)
         return places
  case result of
     [] -> putStrLn "no solution"
     v:_ -> putStrLn (pretty v)

