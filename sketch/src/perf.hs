
import Smten.Prelude
import Smten.Symbolic
import Smten.Control.Monad.State
import Smten.Symbolic.Solver.Yices2

import qualified Smten.Data.Map as Map
import EvalMonad

import Eval
import Syntax
import Options

insertX :: String -> v -> State (Map.Map String v) ()
insertX  k v = modify $ Map.insert k v

lookupX :: String -> State (Map.Map String v) (Maybe v)
lookupX k = gets $ Map.lookup k

perf3 :: IO ()
perf3 = do
  let solver = yices2
      opts = defaultOptions

  r <- run_symbolic solver $ do
     p <- free_Bool
     let env = Map.empty
         run = do
                 let idx = if p then 0 else 1 :: Int
                 case idx of
                 --case ([0, 1 :: Int] !! idx) of
                   0 -> return ()
                   1 -> return ()
                 insertVar "a" (BitV False)
                 insertVar "b" (BitV False)
                 insertVar "c" (BitV False)
                 insertVar "d" (BitV False)
                 lookupVar "d"
         res = runEvalM env Map.empty run
     Smten.Symbolic.assert (Just (Just (BitV True)) == res) 
  putStrLn $ show r

perf4 :: IO ()
perf4 = do
  let solver = yices2
      opts = defaultOptions

  r <- run_symbolic solver $ do
     let a = BinaryE AddOp (ValE (IntV 1)) (ValE $ IntV 1)
         b = ValE $ IntV 4
     e <- mplus (return a) (return b)
     let env = Map.empty
         run = do
            insertVar "a" (IntV 3)
            evalE e
         res = runEvalM env Map.empty run
     Smten.Symbolic.assert (Just (IntV 3) == res) 
  putStrLn $ show r

perf5 :: IO ()
perf5 = do
  let solver = yices2

      spec :: State Int Int
      spec = do
        x1 <- get
        if (x1 < 3)
          then do
            x2 <- get
            return (x2-1)
          else do
            x3 <- get
            if (x3 < 6)
                then return 1
                else get

      sketch :: State Int Int
      sketch = do
        x1 <- get
        if (x1 < 6)
          then do
            put (x1-1)
            x2 <- get
            if (x2 < 2)
              then get
              else sketch
          else get

  r <- run_symbolic solver $ do
     x <- msum (map return [0..50])
     let got = evalState sketch x
         wnt = evalState spec x
     guard (not $ {-# SCC "EQ" #-} got == wnt)
     return (x, (wnt, got))
  putStrLn $ show r

main = perf5


