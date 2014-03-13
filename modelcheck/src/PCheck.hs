
-- | Property Checking
--
-- Based on the paper:
--  "Checking Safety Properties Using Induction and a SAT-Solver"
--      by Mary Sheeran, Satnum Singh, and Gunnar Stalmarck
--   W.A. Hunt, Jr. and S.D. Johnson (Eds.):
--      FMCAD 200, LNCS 1954, pp. 108-125, 2000.

module PCheck (Model(..), pcheck) where

import Smten.Prelude
import Smten.Symbolic
import Smten.Symbolic.SMT

data Model s = Model {
   -- | The initial states of the system
   _I :: s -> Bool,

   -- | The transition relation for the system
   -- Given a state, return a symbolic state representing all possible
   -- next states of the system.
   _T :: s -> Symbolic s,

   -- | Construct a free state
   _S :: Symbolic s
}

-- | pcheck k0 ki m p
-- Verify the property p holds for all states reachable from the initial
-- state.
--   k0 - initial bound to use
--   ki - how much to increment the bound if initial bound is not enough
--   m - the model
--   p - the property to verify
--
-- Returns Nothing if the property holds for all states reachable from the
-- initial state.
--
-- Returns (Just xs) for a sequence of states xs from an initial state to a
-- state not satisfying the property>
--
-- Note: This corresponds to Algorithm 3 of the paper.
pcheck :: (Eq s) => Int -> Int -> Model s -> (s -> Bool) -> SMT (Maybe [s])
pcheck k ki m p = do
   -- Search for a failing path of this length
   ra <- query $ do
      xs <- mkPath m (k+1)
      assert (_I m (head xs) && not (all p xs))
      return xs
   case ra of
     Just xs -> return (Just xs)
     Nothing -> do
       -- Search for a loopfree path of 1 greater length
       rb <- query $ do 
          xs <- mkPath m (k+2)
          assert (_I m (head xs) && all (not . _I m) (tail xs) && loopfree m xs)
       case rb of
          Nothing -> return Nothing
          _ -> do
            -- Search backwards for a loopfree path of 1 greater length
            rc <- query $ do
                xs <- mkPath m (k+2)
                assert (loopfree m xs && all p (init xs) && not (p (last xs)))
            case rc of
              Nothing -> return Nothing
              _ -> pcheck (k+ki) ki m p
                
-- Make a symbolic path of the given length.
mkPath :: Model s -> Int -> Symbolic [s]
mkPath m 0 = return []
mkPath m n = do
  s0 <- _S m
  let extend 0 s = return [s]
      extend k s = do
        s' <- _T m s
        ss <- extend (k-1) s'
        return (s : ss)
  extend (n-1) s0

distinct :: (Eq a) => [a] -> Bool
distinct [] = True
distinct (x:xs) = x `notElem` xs && distinct xs

loopfree :: (Eq s) => Model s -> [s] -> Bool
loopfree m xs = distinct xs

