
-- | Property Checking
--
-- Based on the paper:
--  "Checking Safety Properties Using Induction and a SAT-Solver"
--      by Mary Sheeran, Satnum Singh, and Gunnar Stalmarck
--   W.A. Hunt, Jr. and S.D. Johnson (Eds.):
--      FMCAD 200, LNCS 1954, pp. 108-125, 2000.

{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}
module PCheck (Model(..), pcheck) where

import Smten.Prelude
import Smten.Symbolic
import Smten.Symbolic.SMT

data Model s = Model {
   -- | The initial states of the system
   _I :: s -> Bool,

   -- | The transition relation for the system
   _T :: s -> s -> Bool
}

-- | Verify the given property holds for all states reachable from the initial
-- state.
--
-- Returns Nothing if the property holds for all states reachable from the
-- initial state.
--
-- Returns (Just xs) for a sequence of states xs from an initial state to a
-- state not satisfying the property>
pcheck :: (Eq s) => Model s -> (Symbolic s) -> (s -> Bool) -> SMT (Maybe [s])
pcheck = pcheck' 0

-- | Property checking with an initial guess for the depth of induction needed
-- to solve the property checking problem.
--
-- TODO: Use one of the better algorithms from the paper instead of the
-- simplest (if it helps performance).
pcheck' :: (Eq s) => Int -> Model s -> (Symbolic s) -> (s -> Bool) -> SMT (Maybe [s])
pcheck' k m mkS p = do
   -- Search for a loopfree path from an initial state of length k
   ra <- query $ do 
      xs <- sequence $ replicate (k+1) mkS
      assert (_I m (head xs) && loopfree m xs)
   case ra of
      Nothing -> return Nothing
      _ -> do
        -- Search backwards for a loopfree path from a failing state
        rb <- query $ do
            xs <- sequence $ replicate (k+1) mkS
            assert (loopfree m xs && not (p (last xs)))
        case rb of
          Nothing -> return Nothing
          _ -> do
            -- Search for a failing path of this length
            rc <- query $ do
               xs <- sequence $ replicate (k+1) mkS
               assert (_I m (head xs) && path m xs && not (p (last xs)))
               return xs
            case rc of
              Nothing -> pcheck' (k+1) m mkS p
              Just xs -> return (Just xs)
                

path :: Model s -> [s] -> Bool
path _ [x] = True
path m (a:b:xs) = _T m a b && path m (b:xs)

distinct :: (Eq a) => [a] -> Bool
distinct [] = True
distinct (x:xs) = x `notElem` xs && distinct xs

loopfree :: (Eq s) => Model s -> [s] -> Bool
loopfree m xs = path m xs && distinct xs

