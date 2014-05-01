
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
                 declVar "a" (BitV False)
                 declVar "b" (BitV False)
                 declVar "c" (BitV False)
                 declVar "d" (BitV False)
                 lookupVar "d"
         res = runEvalM env run
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
            declVar "a" (IntV 3)
            evalE e
         res = runEvalM env run
     Smten.Symbolic.assert (Just (IntV 3) == res) 
  putStrLn $ show r

perf5 :: IO ()
perf5 = do
  let solver = yices2
      opts = defaultOptions

  r <- run_symbolic solver $ do
     x <- msum (map return [0..7 :: Int])
     let got = x

         wnt1 =
            let s0 = (True, x)
                m = if x < 3
                     then s0
                     else id s0
--                m = if x < 3
--                       then s0
--                       else id s0
            in case m of
                 (True, v) -> v
          
     guard ({-# SCC "PERF5_GUARD" #-} got == wnt1)
     return x
  putStrLn $ show r

main = perf5


