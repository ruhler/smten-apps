
module AigerPCheck (
    aigercheck,
  ) where

import Smten.Prelude
import Smten.Control.Monad
import Smten.Data.Array
import Smten.Data.Functor
import Smten.Symbolic
import Smten.Symbolic.SMT
import Aiger
import PCheck

data AigerState = AigerState {
  as_input :: Vector,
  as_state :: Vector
} deriving (Eq)

aigermodel :: Aiger -> Model AigerState
aigermodel aig = Model {
  _I = \as -> {-# SCC "AigerModel_I" #-} all not (elems (as_state as)),
  _T = \a1 a2 -> {-# SCC "AigerModel_T" #-}
           case aig_step aig (as_input a1) (as_state a1) of
              (_, ns) -> ns == as_state a2
}

aigerfreestate :: Aiger -> Symbolic AigerState
aigerfreestate aig = do
   inputs <- replicateM (aig_num_inputs aig) free_Bool
   state <- replicateM (aig_num_latches aig) free_Bool
   return $ AigerState {
        as_input = listArray (1, aig_num_inputs aig) inputs,
        as_state = listArray (1, aig_num_latches aig) state
     }

aigerpred :: Aiger -> Literal -> (AigerState -> Bool)
aigerpred aig l = \as -> not (aig_eval aig (as_input as) (as_state as) l)

aigercheck :: Int -> Int -> Aiger -> Literal -> SMT (Maybe [Vector])
aigercheck k0 ki aig l = do
  mstates <- pcheck k0 ki (aigermodel aig) (aigerfreestate aig) (aigerpred aig l)
  return $ (map as_input <$> mstates)

