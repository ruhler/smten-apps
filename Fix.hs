
module Fix (fixN) where

import Debug.Trace

import Control.Monad.State

import Data.Functor((<$>))
import Data.Maybe(fromMaybe)
import qualified Data.Map as Map

import Elem
import SeriRegEx
import RegEx
import CFG

data FS = FS {
    fs_cfgs :: Map.Map ID CFG,
    fs_cache :: Map.Map (ID, Integer) (RegEx Elem)
}

type FixM = State FS

fixidM :: ID -> Integer -> FixM (RegEx Elem)
fixidM x n = do
    fs <- get
    case Map.lookup (x, n) (fs_cache fs) of
        Just v -> return v
        Nothing -> do
            put $ fs { fs_cache = Map.insert (x, n) Empty (fs_cache fs) }
            let r = fromMaybe (error $ "fixid.r: " ++ x) $ Map.lookup x (fs_cfgs fs)
            v <- fixM r n
            modify $ \fs -> fs { fs_cache = Map.insert (x, n) v (fs_cache fs) }
            return v

fixM :: CFG -> Integer -> FixM (RegEx Elem)
fixM r n
  | EpsilonC <- r = return $ if n == 0 then epsilonR else emptyR
  | EmptyC <- r = return emptyR
  | AtomC c <- r = return $ if n == 1 then Atom c else emptyR
  | RangeC a b <- r = return $ if n == 1 then Range a b else emptyR
  | StarC x <- r =
      if n == 0
          then return epsilonR
          else fixM (ConcatC x r) n
  | ConcatC a b <- r =
      let p i = do
            a' <- fixM a i
            if a' == Empty
                then return Empty
                else do
                    b' <- fixM b (n-i)
                    return $ concatR a' b'
      in orsR <$> mapM p [0..n]
  | OrC a b <- r = do
        a' <- fixM a n
        b' <- fixM b n
        return $ orsR [a', b']
  | VariableC id <- r = fixidM id n
  | FixC x n' <- r = if n == n' then fixidM x n else return emptyR

fixN :: Map.Map ID CFG -> ID -> Integer -> RegEx Elem
fixN regs x n = evalState (fixidM x n) $ FS regs Map.empty

