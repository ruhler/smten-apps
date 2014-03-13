
-- | Property Checking
--
-- Based on the paper:
--  "Checking Safety Properties Using Induction and a SAT-Solver"
--      by Mary Sheeran, Satnum Singh, and Gunnar Stalmarck
--   W.A. Hunt, Jr. and S.D. Johnson (Eds.):
--      FMCAD 200, LNCS 1954, pp. 108-125, 2000.

module PCheck (Model(..), pcheck) where

import Smten.Prelude
import Smten.Control.Monad
import Smten.Symbolic
import Smten.Symbolic.SMT

data Model s = Model {
   -- | The initial states of the system
   _I :: s -> Bool,

   -- | The transition relation for the system
   _T :: s -> s -> Bool
}

-- | pcheck k0 ki m mkS p
-- Verify the property p holds for all states reachable from the initial
-- state.
--   k0 - initial bound to use
--   ki - how much to increment the bound if initial bound is not enough
--   m - the model
--   mkS - a constructor for symbolic states
--   p - the property to verify
--
-- Returns Nothing if the property holds for all states reachable from the
-- initial state.
--
-- Returns (Just xs) for a sequence of states xs from an initial state to a
-- state not satisfying the property>
--
-- Note: This corresponds to Algorithm 3 of the paper.
pcheck :: (Eq s) => Int -> Int -> Model s -> (Symbolic s) -> (s -> Bool) -> SMT (Maybe [s])
pcheck k ki m mkS p = do
   -- Search for a failing path of this length
   ra <- query $ do
      xs <- sequence $ replicate (k+1) mkS
      assert (_I m (head xs) && path m xs && not (all p xs))
      return xs
   case ra of
     Just xs -> return (Just xs)
     Nothing -> do
       -- Search for a loopfree path of 1 greater length
       rb <- query $ do 
          xs <- replicateM (k+2) mkS
          assert (_I m (head xs) && all (not . _I m) (tail xs) && loopfree m xs)
       case rb of
          Nothing -> return Nothing
          _ -> do
            -- Search backwards for a loopfree path of 1 greater length
            rc <- query $ do
                xs <- replicateM (k+2) mkS
                assert (loopfree m xs && all p (init xs) && not (p (last xs)))
            case rc of
              Nothing -> return Nothing
              _ -> pcheck (k+ki) ki m mkS p
                

path :: Model s -> [s] -> Bool
path _ [x] = True
path m (a:b:xs) = _T m a b && path m (b:xs)

distinct :: (Eq a) => [a] -> Bool
distinct [] = True
distinct (x:xs) = x `notElem` xs && distinct xs

loopfree :: (Eq s) => Model s -> [s] -> Bool
loopfree m xs = path m xs && distinct xs

