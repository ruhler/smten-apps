
module Fix (FixResult(..), fixN) where

import Smten.Prelude

import Smten.Control.Monad.State.Strict

import Smten.Data.Array
import Smten.Data.Functor
import Smten.Data.Maybe
import qualified Smten.Data.Map as Map

import RegEx
import SCFG

data FS = FS {
    fs_cfgs :: Array SID SCFG,
    fs_cache :: Map.Map (SID, Int) RegEx
}

type FixM = State FS

fixidM :: SID -> Int -> FixM RegEx
fixidM x n = do
    fs <- get
    case Map.lookup (x, n) (fs_cache fs) of
        Just v -> return v
        Nothing -> do
            put $ fs { fs_cache = Map.insert (x, n) Empty (fs_cache fs) }
            let r = fs_cfgs fs ! x
            v <- fixM r n
            modify $ \fs -> fs { fs_cache = Map.insert (x, n) v (fs_cache fs) }
            return v

fixM :: SCFG -> Int -> FixM RegEx
fixM r n =
  case andS [n] (sizeS r) of
     [] -> return Empty
     _ -> do
        res <- fixM' r n
        --traceShow ((r, n), res == Empty) (return ())
        return res

-- Given cfg x, compute fix of x* for all i less than n
-- Returns (xc, fc, res)
--   xc - a cache of Fix sizes of x for 0 < i <= n
--   fc - a cache of fix sizes of (x*) for 0 < i <= n
--   res - the fix size of (x*) for i == n
fixStar :: SCFG -> Int -> FixM (Map.Map Int RegEx, Map.Map Int RegEx, RegEx)
fixStar _ 0 = return (Map.empty, Map.empty, epsilonR)
fixStar x n = do
    (xc, fc, _) <- fixStar x (n-1)
    xn <- fixM x n
    let p = \i -> do
          case (Map.lookup i xc) of
            Nothing -> Empty
            Just a' -> concatR a' (fromMaybe Empty (Map.lookup (n-i) fc))
    let res = orsR (xn : map p (andS [1..(n-1)] (sizeS x)))
    return (Map.insert n xn xc, Map.insert n res fc, res)

fixM' :: SCFG -> Int -> FixM RegEx
fixM' r n = 
  case r of
     EpsilonS -> return $ if n == 0 then epsilonR else emptyR
     EmptyS -> return emptyR
     AtomS c -> return $ if n == 1 then Atom c else emptyR
     RangeS a b -> return $ if n == 1 then Range a b else emptyR
     StarS x _ -> do
        (_, _, r) <- fixStar x n
        return r
     ConcatS a b _ ->
       let p = \i -> do
             a' <- fixM a i
             case a' of
                Empty -> return a'
                _ -> do
                     b' <- fixM b (n-i)
                     return $ concatR a' b'
       in orsR <$> mapM p (andS [0..n] (sizeS a))
     OrS a b _ -> do
         a' <- fixM a n
         b' <- fixM b n
         return $ orsR [a', b']
     VariableS id _ -> do
        x <- fixidM id n
        case x of
          Empty -> return x
          _ -> return $ Variable n id
     FixS x n' -> if n == n' then fixidM x n else return emptyR

data FixResult = FixResult {
    fr_regs :: Array (RID, Int) RegEx,
    fr_top :: RegEx
}

fixN :: Array SID SCFG -> SID -> Int -> FixResult
fixN regs x n = {-# SCC "FixN" #-}
  let (r, s) = runState (fixidM x n) $ FS regs Map.empty
      maxsid = snd (bounds regs)
      bnds = ((0, 0), (maxsid, n))
      elems = Map.toList (fs_cache s)
  in --traceShow r $
      FixResult {    
        fr_regs = array bnds elems,
        fr_top = r
      }

