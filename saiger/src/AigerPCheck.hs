
module AigerPCheck (
    aigercheck,
  ) where

import Smten.Prelude
import Smten.Control.Monad
import Smten.Data.Array
import Smten.Data.Functor
import Smten.Searches
import Aiger
import PCheck

data AigerState = AigerState {
  as_input :: Vector,
  as_state :: Vector
} deriving (Eq)

aigermodel :: Aiger -> Model AigerState
aigermodel aig = Model {
  _I = \as -> {-# SCC "AigerModel_I" #-} all not (elems (as_state as)),
  _T = \a -> {-# SCC "AigerModel_T" #-} do
          ni <- freeVector (aig_num_inputs aig)
          case aig_step aig (as_input a) (as_state a) of
             (_, ns) -> return (AigerState ni ns),
  _S = aigerfreestate aig
}

freeVector :: Int -> Space Vector
freeVector n = do
  xs <- replicateM n free_Bool
  return $ listArray (1, n) xs

aigerfreestate :: Aiger -> Space AigerState
aigerfreestate aig = do
   inputs <- freeVector (aig_num_inputs aig)
   state <- freeVector (aig_num_latches aig)
   return $ AigerState {
        as_input = inputs,
        as_state = state
     }

aigerpred :: Aiger -> Literal -> (AigerState -> Bool)
aigerpred aig l = \as -> not (aig_eval aig (as_input as) (as_state as) l)

aigercheck :: Int -> Int -> Aiger -> Literal -> Searches (Maybe [Vector])
aigercheck k0 ki aig l = do
  mstates <- pcheck k0 ki (aigermodel aig) (aigerpred aig l)
  return $ (map as_input <$> mstates)

