
module Fix (FixResult(..), fixN) where

import Smten.Prelude

import Smten.Control.Monad.State.Strict

import Smten.Data.Functor
import Smten.Data.Maybe
import qualified Smten.Data.Map as Map

import Smten.Debug.Trace

import RegEx
import CFG

data FS = FS {
    fs_cfgs :: Map.Map ID CFG,
    fs_cache :: Map.Map (RID, Int) RegEx,
    fs_rids :: Map.Map ID RID,
    fs_nrid :: RID
}

type FixM = State FS

getridM :: ID -> FixM RID
getridM x = do
    fs <- get
    case Map.lookup x (fs_rids fs) of
        Just v -> return v
        Nothing -> do
            let v = fs_nrid fs
            put $ fs { fs_nrid = v+1, fs_rids = Map.insert x v (fs_rids fs) }
            return v

fixidM :: ID -> Int -> FixM (RegEx, RID)
fixidM x n = do
    rid <- getridM x
    fs <- get
    case Map.lookup (rid, n) (fs_cache fs) of
        Just v -> return (v, rid)
        Nothing -> do
            put $ fs { fs_cache = Map.insert (rid, n) Empty (fs_cache fs) }
            let r = fromMaybe (error $ "fixid.r: " ++ x) $ Map.lookup x (fs_cfgs fs)
            v <- fixM r n
            modify $ \fs -> fs { fs_cache = Map.insert (rid, n) v (fs_cache fs) }
            return (v, rid)

fixM :: CFG -> Int -> FixM RegEx
fixM r n = do
    reg <- fixM' r n
    --traceShow ((r, n), reg == Empty) $ return reg
    return reg

-- Given cfg x, compute fix of x* for all i less than n
-- Returns (xc, fc, res)
--   xc - a cache of Fix sizes of x for 0 < i <= n
--   fc - a cache of fix sizes of (x*) for 0 < i <= n
--   res - the fix size of (x*) for i == n
fixStar :: CFG -> Int -> FixM (Map.Map Int RegEx, Map.Map Int RegEx, RegEx)
fixStar _ 0 = return (Map.empty, Map.empty, epsilonR)
fixStar x n = do
    (xc, fc, _) <- fixStar x (n-1)
    xn <- fixM x n
    let p = \i -> do
          case (Map.lookup i xc) of
            Nothing -> Empty
            Just Empty -> Empty
            Just a' -> concatR a' (fromMaybe Empty (Map.lookup (n-i) fc))
    let res = orsR (xn : map p [1..(n-1)])
    return (Map.insert n xn xc, Map.insert n res fc, res)

fixM' :: CFG -> Int -> FixM RegEx
fixM' r n = 
  case r of
     EpsilonC -> return $ if n == 0 then epsilonR else emptyR
     EmptyC -> return emptyR
     AtomC c -> return $ if n == 1 then Atom c else emptyR
     RangeC a b -> return $ if n == 1 then Range a b else emptyR
     StarC x -> do
        (_, _, r) <- fixStar x n
        return r
     ConcatC a b ->
       let p = \i -> do
             a' <- fixM a i
             case a' of
                Empty -> return a'
                _ -> do
                     b' <- fixM b (n-i)
                     return $ concatR a' b'
       in orsR <$> mapM p [0..n]
     OrC a b -> do
         a' <- fixM a n
         b' <- fixM b n
         return $ orsR [a', b']
     VariableC id -> do
        (x, rid) <- fixidM id n
        case x of
          Empty -> return x
          _ -> return $ Variable n rid
     FixC x n' -> if n == n' then fst <$> fixidM x n else return emptyR

data FixResult = FixResult {
    fr_regbound :: ((RID, Int), (RID, Int)),
    fr_regs :: [((RID, Int), RegEx)],
    fr_top :: RegEx
}

fixN :: Map.Map ID CFG -> ID -> Int -> FixResult
fixN regs x n = {-# SCC "FixN" #-}
  let (r, s) = runState (fst <$> fixidM x n) $ FS regs Map.empty Map.empty 0
  in FixResult {    
        fr_regbound = ((0, 0), ((fs_nrid s)-1, n)),
        fr_regs = Map.toList (fs_cache s),
        fr_top = r
      }

