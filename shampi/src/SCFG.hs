
-- | Sized CFGs
module SCFG (
    SID, Sizes, SCFG(..), andS,
    cfgsS, sizeS,
    ) where

import Smten.Prelude
import Smten.Control.Monad.State
import Smten.Data.Array
import Smten.Data.Maybe
import qualified Smten.Data.Map as Map
import CFG

type SID = Int

-- Sizes: a (potentially infinite) ordered list of the possible lengths
-- of a string a given CFG can match against.
type Sizes = [Int]

-- | CFG annotated with sizes
data SCFG =
     EpsilonS
   | EmptyS
   | AtomS Char
   | RangeS Char Char
   | StarS SCFG Sizes
   | ConcatS SCFG SCFG Sizes
   | OrS SCFG SCFG Sizes
   | VariableS SID Sizes
   | FixS SID Int
    deriving (Show)

-- This is a constant time operation.
sizeS :: SCFG -> Sizes
sizeS EpsilonS = [0]
sizeS EmptyS = []
sizeS (AtomS {}) = [1]
sizeS (RangeS {}) = [1]
sizeS (StarS _ x) = x
sizeS (ConcatS _ _ x) = x
sizeS (OrS _ _ x) = x
sizeS (VariableS _ x) = x
sizeS (FixS _ x) = [x]

orS :: Sizes -> Sizes -> Sizes
orS x [] = x
orS [] y = y
orS x_@(x:xs) y_@(y:ys)
 | x == y = x : orS xs ys
 | x < y = x : orS xs y_
 | otherwise = y : orS x_ ys

andS :: Sizes -> Sizes -> Sizes
andS x [] = []
andS [] y = []
andS x_@(x:xs) y_@(y:ys)
 | x == y = x : andS xs ys
 | x < y = andS xs y_
 | otherwise = andS x_ ys

starS :: Sizes -> Sizes
starS xs = 0 : plusS xs

plusS :: Sizes -> Sizes
plusS [] = []
plusS (x:xs) = orS (seqS x) (plusS xs)

seqS :: Int -> Sizes
seqS x = enumFromThen x (x+x)

concatS :: Sizes -> Sizes -> Sizes
concatS [] _ = []
concatS (x:xs) ys = orS (map (+ x) ys) (concatS xs ys)

data SS = SS {
    ss_cfgs :: Map.Map ID CFG,
    ss_cache :: Map.Map SID SCFG,
    ss_sids :: Map.Map ID SID,
    ss_nsid :: SID
}

type SizeM = State SS

getsidM :: ID -> SizeM SID
getsidM x = do
    ss <- get
    case Map.lookup x (ss_sids ss) of
        Just v -> return v
        Nothing -> do
            let v = ss_nsid ss
            put $ ss { ss_nsid = v+1, ss_sids = Map.insert x v (ss_sids ss) }
            return v

cfgidM :: ID -> SizeM (SCFG, SID)
cfgidM x = do
    sid <- getsidM x
    ss <- get
    case Map.lookup sid (ss_cache ss) of
        Just v -> return (v, sid)
        Nothing -> do
            put $ ss { ss_cache = Map.insert sid EmptyS (ss_cache ss) }
            let r = fromMaybe (error $ "cfgid.r: " ++ x) $ Map.lookup x (ss_cfgs ss)
            v <- cfgM r
            modify $ \ss -> ss { ss_cache = Map.insert sid v (ss_cache ss) }
            return (v, sid)

cfgM :: CFG -> SizeM SCFG
cfgM r = 
  case r of
     EpsilonC -> return EpsilonS
     EmptyC -> return EmptyS
     AtomC c -> return $ AtomS c
     RangeC a b -> return $ RangeS a b
     StarC x -> do
        x' <- cfgM x
        return $ StarS x' (starS (sizeS x'))
     ConcatC a b -> do
        a' <- cfgM a
        b' <- cfgM b
        return $ ConcatS a' b' (concatS (sizeS a') (sizeS b'))
     OrC a b -> do
         a' <- cfgM a
         b' <- cfgM b
         return $ OrS a' b' (orS (sizeS a') (sizeS b'))
     VariableC id -> do
        (x, sid) <- cfgidM id
        return $ VariableS sid (sizeS x)
     FixC x n -> do
        (_, x') <- cfgidM x
        return $ FixS x' n

cfgsM :: Map.Map ID CFG -> SizeM ()
cfgsM regs = mapM_ cfgidM (Map.keys regs)

cfgsS :: Map.Map ID CFG -> (Map.Map ID SID, Array SID SCFG)
cfgsS regs =
  let s = execState (cfgsM regs) (SS regs Map.empty Map.empty 0)
      bounds = (0, ss_nsid s - 1)
      elems = Map.assocs (ss_cache s)
  in (ss_sids s, array bounds elems)

