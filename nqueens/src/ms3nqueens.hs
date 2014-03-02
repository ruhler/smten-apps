
-- Implementation of n-queens using MiniSat
-- Implemented in Haskell, using MiniSat.FFI from smten-minisat.

import Data.Functor
import System.Environment
import System.Exit
import System.IO

import Smten.Runtime.MiniSat.FFI

import Block


nqueens :: Int -> IO (Maybe [Int])
nqueens n = do
  s <- c_minisat_new
  places <- blockM (c_minisat_var s) n n
  let orM :: [MSExpr] -> IO MSExpr
      orM [] = c_minisat_false s
      orM (x:xs) = do
        xs' <- orM xs
        c_minisat_or s x xs'

      andM :: [IO MSExpr] -> IO MSExpr
      andM [] = c_minisat_true s
      andM (x:xs) = do
        x' <- x
        xs' <- andM xs
        c_minisat_and s x' xs'

      allM :: (a -> IO MSExpr) -> [a] -> IO MSExpr
      allM f xs = andM (map f xs)

      noconf :: [MSExpr] -> IO MSExpr
      noconf [] = c_minisat_true s
      noconf (x:xs) = do
        let nc :: MSExpr -> MSExpr -> IO MSExpr
            nc a b = do
              na <- c_minisat_not s a
              nb <- c_minisat_not s b
              c_minisat_or s na nb
        x' <- allM (nc x) xs
        xs' <- noconf xs
        c_minisat_and s x' xs'

      islegal :: Block MSExpr -> IO MSExpr
      islegal places = andM [
          allM orM (rows places),
          allM noconf (rows places),
          allM noconf (cols places),
          allM noconf (pdiags places),
          allM noconf (ndiags places)]
  p <- islegal places
  c_minisat_assert s p
  r <- c_minisat_check s
  case r of
    0 -> return Nothing
    1 -> do
      let get x = (== 1) <$> c_minisat_getvar s x
      places' <- blockMapM get places
      return $ Just (map colof (rows places'))

colof :: [Bool] -> Int
colof [] = error "no queen found in given row"
colof (True:_) = 0
colof (False:xs) = 1 + colof xs

main :: IO ()
main = do
  args <- getArgs
  if "--help" `elem` args
     then putStrLn usage >> exitSuccess
     else return ()

  n <- case lookupn args of
          Nothing -> fail $ "no board size input\n" ++ usage 
          Just v -> return v

  -- If n is less than zero, we try all n starting from 0 to (-n) and print a
  -- single line for each solution.
  if (n < 0)
    then do
        hSetBuffering stdout LineBuffering
        flip mapM_ [0..(negate n)] $ \n -> do
            nqueens n >>= print
    else do
      mxs <- nqueens n
      case mxs of
        Nothing -> putStrLn "no solution"
        Just xs -> putStrLn $ pretty n xs

usage :: String
usage = "ms3nqueens <n>"

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

