
-- | Property Checking
--
-- Based on the paper:
--  "Checking Safety Properties Using Induction and a SAT-Solver"
--      by Mary Sheeran, Satnum Singh, and Gunnar Stalmarck
--   W.A. Hunt, Jr. and S.D. Johnson (Eds.):
--      FMCAD 200, LNCS 1954, pp. 108-125, 2000.

module PCheck (Model(..), Alg(..), pcheck) where

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

data Alg = A1 | A2
    | A3 Int Int      -- A3 k0 kincr
    deriving (Eq, Show)

-- | Verify the given property holds for all states reachable from the initial
-- state.
--
-- Returns Nothing if the property holds for all states reachable from the
-- initial state.
--
-- Returns (Just xs) for a sequence of states xs from an initial state to a
-- state not satisfying the property>
pcheck :: (Eq s) => Alg -> Model s -> (Symbolic s) -> (s -> Bool) -> SMT (Maybe [s])
pcheck A1 = pcheck1' 0
pcheck A2 = pcheck2' 0
pcheck (A3 k0 kincr) = pcheck3' k0 kincr

-- | Property checking with an initial guess for the depth of induction needed
-- to solve the property checking problem.
-- Algorithm 1
pcheck1' :: (Eq s) => Int -> Model s -> (Symbolic s) -> (s -> Bool) -> SMT (Maybe [s])
pcheck1' k m mkS p = do
   -- Search for a loopfree path from an initial state of length k
   ra <- query $ do 
      xs <- replicateM (k+1) mkS
      assert (_I m (head xs) && loopfree m xs)
   case ra of
      Nothing -> return Nothing
      _ -> do
        -- Search backwards for a loopfree path from a failing state
        rb <- query $ do
            xs <- replicateM (k+1) mkS
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
              Nothing -> pcheck1' (k+1) m mkS p
              Just xs -> return (Just xs)

-- | Property checking with an initial guess for the depth of induction needed
-- to solve the property checking problem.
-- Algorithm 2
pcheck2' :: (Eq s) => Int -> Model s -> (Symbolic s) -> (s -> Bool) -> SMT (Maybe [s])
pcheck2' k m mkS p = do
   -- Search for a loopfree path from an initial state of length k
   ra <- query $ do 
      xs <- replicateM (k+1) mkS
      assert (_I m (head xs) && all (not . _I m) (tail xs) && loopfree m xs)
   case ra of
      Nothing -> return Nothing
      _ -> do
        -- Search backwards for a loopfree path from a failing state
        rb <- query $ do
            xs <- replicateM (k+1) mkS
            assert (loopfree m xs && all p (init xs) && not (p (last xs)))
        case rb of
          Nothing -> return Nothing
          _ -> do
            -- Search for a failing path of this length
            rc <- query $ do
               xs <- sequence $ replicate (k+1) mkS
               assert (_I m (head xs) && path m xs && not (p (last xs)))
               return xs
            case rc of
              Nothing -> pcheck2' (k+1) m mkS p
              Just xs -> return (Just xs)

-- | Property checking with an initial guess for the depth of induction needed
-- to solve the property checking problem, and an increment.
-- Algorithm 3
pcheck3' :: (Eq s) => Int -> Int -> Model s -> (Symbolic s) -> (s -> Bool) -> SMT (Maybe [s])
pcheck3' k kincr m mkS p = do
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
              _ -> pcheck3' (k+kincr) kincr m mkS p
                

path :: Model s -> [s] -> Bool
path _ [x] = True
path m (a:b:xs) = _T m a b && path m (b:xs)

distinct :: (Eq a) => [a] -> Bool
distinct [] = True
distinct (x:xs) = x `notElem` xs && distinct xs

loopfree :: (Eq s) => Model s -> [s] -> Bool
loopfree m xs = path m xs && distinct xs

