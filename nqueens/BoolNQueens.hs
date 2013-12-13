
module BoolNQueens (bool_nqueens, bool_nqueens_test) where

import Smten.Prelude
import Smten.Control.Monad
import Smten.Control.Monad.State
import Smten.Control.Monad.Trans
import Smten.Data.Array
import Smten.Data.Char
import Smten.Symbolic
import Smten.Symbolic.SMT

import Block

type Placement = Block Bool


-- Return True if exactly one of the inputs is set.
oneset :: [Bool] -> Bool
oneset =
  -- s - at least some bit has been seen set.
  -- m - multiple bits have been seen set.
  let oneset' s m [] = s && not m
      oneset' s m (x:xs) = oneset' (s || x) ((s && x) || m) xs
  in oneset' False False

-- Return True if multiple inputs are set
multiset :: [Bool] -> Bool
multiset =
  -- s - at least some bit has been seen set.
  -- m - multiple bits have been seen set.
  let multiset' s m [] = m
      multiset' s m (x:xs) = multiset' (s || x) ((s && x) || m) xs
  in multiset' False False
 
islegal :: Placement -> Bool
islegal places = and [
  all oneset (rows places),
  all oneset (cols places),
  all (not . multiset) (pdiags places),
  all (not . multiset) (ndiags places)]

pretty :: Placement -> String
pretty places = unlines (map (map (\x -> if x then '♛' else '.')) (rows places))

bool_nqueens :: Int -> SMT ()
bool_nqueens n = do
  result <- query $ do
         places <- blockM free_Bool n n
         assert (islegal places)
         return places
  liftIO $ case result of
              Nothing -> putStrLn "no solution"
              Just v -> putStrLn (pretty v)

bool_nqueens_test :: IO ()
bool_nqueens_test = do
  putStrLn $ "oneset [F, F, T, F, F] = " ++ show (oneset [False, False, True, False, False])
  putStrLn $ "oneset [F, F, F, F, F] = " ++ show (oneset [False, False, False, False, False])
  putStrLn $ "oneset [F, T, T, F, F] = " ++ show (oneset [False, True, True, False, False])
  putStrLn $ "oneset [F, T, F, F, T] = " ++ show (oneset [False, True, False, False, True])
  putStrLn $ "multiset [F, F, T, F, F] = " ++ show (multiset [False, False, True, False, False])
  putStrLn $ "multiset [F, F, F, F, F] = " ++ show (multiset [False, False, False, False, False])
  putStrLn $ "multiset [F, T, T, F, F] = " ++ show (multiset [False, True, True, False, False])
  putStrLn $ "multiset [F, T, F, F, T] = " ++ show (multiset [False, True, False, False, True])
  putStrLn $ "islegal [.X..,...X,X...,..X.] = " ++ show (islegal [[False, True, False, False], [False, False, False, True], [True, False, False, False], [False, False, True, False]])
  putStrLn $ "blockM = " ++ show dummyblock

dummyblock :: Block Char
dummyblock = 
  let mkc = do
        i <- get
        put (i+1)
        return (chr $ ord 'A' + i)
  in evalState (blockM mkc 4 4) 0

