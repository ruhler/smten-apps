
module AigerPCheck (
    aigercheck,
  ) where

import Smten.Prelude
import Smten.Data.Array
import Smten.Data.Functor
import Smten.Symbolic
import Smten.Symbolic.SMT
import Aiger
import PCheck

data AigerState = AigerState {
  as_input :: Vector,
  as_state :: Vector
}

instance Eq AigerState where
  (==) a b = (as_input a == as_input b) && (as_state a == as_state b)

aigermodel :: Aiger -> Model AigerState
aigermodel aig = Model {
  _I = \as -> all (== False) (elems (as_state as)),
  _T = \a1 a2 ->
          let (_, _, ns) = aig_step aig (as_input a1) (as_state a1)
          in ns == (as_state a2)
}

aigerfreestate :: Aiger -> Symbolic AigerState
aigerfreestate aig = do
   inputs <- sequence (replicate (aig_num_inputs aig) free_Bool)
   state <- sequence (replicate (aig_num_latches aig) free_Bool)
   return $ AigerState {
        as_input = listArray (1, aig_num_inputs aig) inputs,
        as_state = listArray (1, aig_num_latches aig) state
     }

aigerpred :: Aiger -> Literal -> (AigerState -> Bool)
aigerpred aig l = \as -> not (aig_eval aig (as_input as) (as_state as) l)

aigercheck :: Aiger -> Literal -> SMT (Maybe [Vector])
aigercheck aig l = do
  mstates <- pcheck (aigermodel aig) (aigerfreestate aig) (aigerpred aig l)
  return $ do
     states <- mstates
     return (map as_input states)

