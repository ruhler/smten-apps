
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
      opts = defaultOptions

  r <- run_symbolic solver $ do
     let spec = BlockS [IfS (BinaryE LtOp (VarE "x") (ValE (IntV 3))) (ReturnS (BinaryE SubOp (VarE "x") (ICastE IntT (ValE (BitV True))))) (BlockS []),IfS (BinaryE LtOp (VarE "x") (ValE (IntV 6))) (ReturnS (ICastE IntT (ValE (BitV True)))) (BlockS []),ReturnS (VarE "x")]
         sketch = BlockS [WhileS (BinaryE LtOp (VarE "x") (ValE (IntV 6))) (BlockS [UpdateS (VarLV "x") (BinaryE SubOp (VarE "x") (ICastE IntT (ValE (BitV True)))),IfS (BinaryE LtOp (VarE "x") (ValE (IntV 2))) (ReturnS (VarE "x")) (BlockS [])]),ReturnS (VarE "x")]
     x <- msum (map return [0..8])
     let got = runEvalM Map.empty Map.empty $ do
            insertVar "x" (IntV x)
            evalS sketch
         wnt = runEvalM Map.empty Map.empty $ do
            insertVar "x" (IntV x)
            evalS spec
     guard ({-# SCC "PERF5_GUARD" #-} got == wnt)
     return x
  putStrLn $ show r

main = perf5


