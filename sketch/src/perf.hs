
import Smten.Prelude
import Smten.Symbolic
import Smten.Control.Monad.State
import Smten.Symbolic.Solver.Yices2

import qualified Smten.Data.Map as Map
import EvalMonad

import Smten.Data.Functor
import Smten.Data.Maybe

import Eval
import Syntax

insertX :: String -> v -> State (Map.Map String v) ()
insertX  k v = modify $ Map.insert k v

lookupX :: String -> State (Map.Map String v) (Maybe v)
lookupX k = gets $ Map.lookup k

main :: IO ()
main = do
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

