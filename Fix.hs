
module Fix (fixN) where

import Debug.Trace

import Control.Monad.State

import Data.Functor((<$>))
import Data.Maybe(fromMaybe)
import qualified Data.Map as Map

import SeriRegEx
import RegEx
import Elem

data FS = FS {
    fs_regs :: Map.Map ID (RegEx Elem),
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
            let r = fromMaybe (error $ "fixid.r: " ++ x) $ Map.lookup x (fs_regs fs)
            v <- fixM r n
            modify $ \fs -> fs { fs_cache = Map.insert (x, n) v (fs_cache fs) }
            return v

fixM :: RegEx Elem -> Integer -> FixM (RegEx Elem)
fixM r n
  | Epsilon <- r = return $ if n == 0 then Epsilon else Empty
  | Empty <- r = return Empty
  | Atom {} <- r = return $ if n == 1 then r else Empty
  | Range {} <- r = return $ if n == 1 then r else Empty
  | Star x <- r =
      if n == 0
          then return Epsilon
          else fixM (Concat x r) n
  | Concat a b <- r =
      let p i = do
            a' <- fixM a i
            if a' == Empty
                then return Empty
                else do
                    b' <- fixM b (n-i)
                    return $ concatR a' b'
      in orsR <$> mapM p [0..n]
  | Or a b <- r = do
        a' <- fixM a n
        b' <- fixM b n
        return $ orR a' b'
  | Variable id <- r = fixidM id n
  | Fix x n' <- r = if n == n' then fixM x n else return Empty

fixN :: Map.Map ID (RegEx Elem) -> ID -> Integer -> RegEx Elem
fixN regs x n = evalState (fixidM x n) $ FS regs Map.empty

