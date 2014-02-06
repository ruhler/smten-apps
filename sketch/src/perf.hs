
import Smten.Prelude
import Smten.Symbolic
import Smten.Symbolic.Solver.Yices2

import qualified Smten.Data.Map as Map
import EvalMonad

import Eval
import Sketch

main :: IO ()
main = do
  let solver = yices2
      opts = defaultOptions

  r <- run_symbolic solver $ do
     inv <- free_Bool
     let env = Map.empty
         run = do
                 let a = (if inv then 1 else 0) :: Int
                 case a of
                    0 -> return ()
                    1 -> return ()
                 insertVar "y" (BitV False)
                 insertVar "v" (BitV False)
                 insertVar "w" (BitV False)
                 insertVar "x" (BitV False)
                 mval <- lookupVar "x"
                 case mval of
                    Just v -> return v
         res = runEvalM env run
     Smten.Symbolic.assert (Just (BitV inv) == res) 
  putStrLn $ show r

