
-- Implementation of n-queens using MiniSat
-- Implemented in Haskell, using BoolFF from smten
-- with a customized build function.

import qualified Data.HashTable.IO as H
import Data.Functor
import System.Environment
import System.Exit
import System.IO

import Smten.Runtime.MiniSat.FFI
import Smten.Runtime.FreeID
import Smten.Runtime.Formula.Finite
import Smten.Runtime.Model
import Smten.Runtime.BuildCache

import Block

solve :: BoolFF -> IO (Maybe Model)
solve p = do
  s <- c_minisat_new
  vars <- H.new :: IO (H.BasicHashTable FreeID MSExpr)
  key <- newKey
  let build :: BoolFF -> IO MSExpr
      build (VarFF x c) = cached c key $ do
        mv <- H.lookup vars x
        case mv of
            Nothing -> do   
              v <- c_minisat_var s
              H.insert vars x v
              return v
            Just v -> return v
      build TrueFF = c_minisat_true s
      build FalseFF = c_minisat_false s
      build (NotFF a c) = cached c key $ do
        a' <- build a
        c_minisat_not s a'
      build (AndFF a b c) = cached c key $ do
        a' <- build a
        b' <- build b
        c_minisat_and s a' b'
      build (OrFF a b c) = cached c key $ do
        a' <- build a
        b' <- build b
        c_minisat_or s a' b'     
      build x = error "Unsupported formula in build"

  p' <- build p 
  c_minisat_assert s p'
  r <- c_minisat_check s
  case r of
   0 -> do
     c_minisat_delete s
     return Nothing
   1 -> do
     let f (nm, v) = do
           v' <- c_minisat_getvar s v
           return (nm, BoolA $ v' == 1)
     vs <- H.toList vars
     mvs <- mapM f vs
     c_minisat_delete s
     Just <$> model mvs
            

nqueens :: Int -> IO (Maybe [Int])
nqueens n = do
  places <- blockM (varFF <$> fresh) n n
  let orM :: [BoolFF] -> BoolFF
      orM [] = falseFF
      orM (x:xs) = orFF x (orM xs)

      andM :: [BoolFF] -> BoolFF
      andM [] = trueFF
      andM (x:xs) = andFF x (andM xs)

      allM :: (a -> BoolFF) -> [a] -> BoolFF
      allM f xs = andM (map f xs)

      noconf :: [BoolFF] -> BoolFF
      noconf [] = trueFF
      noconf (x:xs) =
        let nc a b = orFF (notFF a) (notFF b)
        in andFF (allM (nc x) xs) (noconf xs)

      islegal :: Block BoolFF -> BoolFF
      islegal places = andM [
          allM orM (rows places),
          allM noconf (rows places),
          allM noconf (cols places),
          allM noconf (pdiags places),
          allM noconf (ndiags places)]
  let p = islegal places
  r <- solve p
  case r of
    Nothing -> return Nothing
    Just m -> do
      let get (VarFF x _) = lookupBool m x
          places' = blockMap get places
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

