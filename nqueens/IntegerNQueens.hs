

-- | Implementation of nqueens based on the Integer type.
module IntegerNQueens( integer_nqueens ) where

import Smten.Prelude
import Smten.Control.Monad
import Smten.Control.Monad.Trans
import Smten.Symbolic
import Smten.Symbolic.SMT

-- Placement: A list of locations (row, col) for each queen.
-- Indices go from 0 to n-1
type Placement = [(Integer, Integer)]

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
pretty places = unlines [[if (r, c) `elem` places then 'X' else '.'
                | c <- [0..(toInteger $ length places - 1)]]
                | r <- [0..(toInteger $ length places - 1)]]

mkcol :: Int -> Symbolic Integer
mkcol n = do
   x <- free_Integer
   assert (x >= 0 && x < toInteger n)
   return x

integer_nqueens :: Int -> SMT ()
integer_nqueens n = do
  result <- query $ do
         let rows = [0..(toInteger n-1)]
         cols <- sequence $ replicate n (mkcol n)
         let places = zip rows cols
         assert (islegal places)
         return places
  liftIO $ case result of
              Nothing -> putStrLn "no solution"
              Just v -> putStrLn (pretty v)

