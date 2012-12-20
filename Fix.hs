
module Fix where

import Control.Monad.State

import Data.Functor
import Data.Maybe
import qualified Data.Map as Map
import qualified Map as SMap

import SeriRegEx
import SeriCFG

data FS = FS {
    fs_cfgs :: SMap.Map ID CFG,
    fs_cache :: Map.Map (ID, Integer) RegEx
}

type FixM = State FS

fixidM :: ID -> Integer -> FixM RegEx
fixidM x n = do
    fs <- get
    case Map.lookup (x, n) (fs_cache fs) of
        Just v -> return v
        Nothing -> do
            put $ fs { fs_cache = Map.insert (x, n) Empty (fs_cache fs) }
            let r = fromMaybe (error $ "fixid.r: " ++ x) $ SMap.map_lookup x (fs_cfgs fs)
            v <- fixM r n
            modify $ \fs -> fs { fs_cache = Map.insert (x, n) v (fs_cache fs) }
            return v

fixM :: CFG -> Integer -> FixM RegEx
fixM r n =
  case r of
     EpsilonC -> return $ if n == 0 then epsilonR else emptyR
     EmptyC -> return emptyR
     AtomC c -> return $ if n == 1 then Atom c else emptyR
     RangeC a b -> return $ if n == 1 then Range a b else emptyR
     StarC x ->
       if n == 0
           then return epsilonR
           else fixM (ConcatC x r) n
     ConcatC a b ->
       let p = \i -> do
             a' <- fixM a i
             if a' == Empty
                 then return Empty
                 else do
                     b' <- fixM b (n-i)
                     return $ concatR a' b'
       in orsR <$> mapM p [0..n]
     OrC a b -> do
         a' <- fixM a n
         b' <- fixM b n
         return $ orsR [a', b']
     VariableC id -> do
        x <- fixidM id n
        return $
          case x of
            Empty -> Empty
            _ -> Variable n id
     FixC x n' -> if n == n' then fixidM x n else return emptyR

fixN :: SMap.Map ID CFG -> ID -> Integer -> (SMap.Map (ID, Integer) RegEx, RegEx)
fixN regs x n =
  let (r, s) = runState (fixidM x n) $ FS regs Map.empty
  in (SMap.map_fromList . filter ((/= Empty) . snd) . Map.toList $ fs_cache s, r)

