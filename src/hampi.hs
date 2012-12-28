
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

import System.Environment
import System.Timeout
import Control.Monad.State

import Debug.Trace

import Map
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Functor ((<$>))
import Data.List (genericLength)

import Seri.Type
import Seri.Exp
import Seri.ExpH
import Seri.Ppr
import qualified Seri.HaskellF.Symbolic as S
import Seri.HaskellF.Query
import Seri.SMT.Query
import Seri.SMT.Yices.Yices2
import Seri.SMT.Yices.Yices1
import Seri.SMT.STP.STP
import qualified Seri.HaskellF.Lib.Prelude as S
import qualified SHampi as S

import Elem
import RegEx
import CFG
import Hampi
import Grammar
import Fix

derive_SeriT ''RegEx
derive_SeriEH ''RegEx

instance S.SeriS RegEx S.RegEx where
    seriS Epsilon = S.Epsilon
    seriS Empty = S.Empty
    seriS (Atom c) = S.Atom (S.seriS c)
    seriS (Range a b) = S.Range (S.seriS a) (S.seriS b)
    seriS (Concat a b c) = S.Concat (S.seriS a) (S.seriS b) (S.seriS c)
    seriS (Or a b c) = S.Or (S.seriS a) (S.seriS b) (S.seriS c)
    seriS (Variable a b) = S.Variable (S.seriS a) (S.seriS b)

instance (S.SeriS ca fa, S.SeriS cb fb) => S.SeriS (ca, cb) (S.Tuple2__ fa fb) where
    seriS (a, b) = S.Tuple2__ (S.seriS a) (S.seriS b)

derive_SeriT ''Map
derive_SeriEH ''Map

instance (S.SeriS ck fk, S.SeriS cv fv) => S.SeriS (Map ck cv) (S.Map fk fv) where
    seriS Tip = S.Tip
    seriS (Bin a b c d e) = S.Bin (S.seriS a) (S.seriS b) (S.seriS c) (S.seriS d) (S.seriS e)
    

type S_Elem = S.Integer
type S_String = S.List__ S_Elem

freevar :: Integer -> Query S_String
freevar = qS . S.frees . S.seriS

-- Make a hampi assertion.
hassert :: M.Map ID (Integer, S_String) -> M.Map ID CFG -> Assertion -> Query ()
hassert vals cfgs (AssertIn v b r) =
    let (vlen, vstr) = fromMaybe (error $ "val " ++ v ++ " not found") $ M.lookup v vals
        (regs, reg) = {-# SCC "FixN" #-} fixN cfgs r vlen
        reg' = {-# SCC "SeriS" #-} S.seriS reg
        regs' = {-# SCC "SeriS" #-} S.seriS regs
        b' = S.seriS b
        p = {-# SCC "AssertIn" #-} S.assertIn regs' vstr b' reg'
    in assertS p
hassert vals _ (AssertEquals v b x) =
    let vstr = snd $ fromMaybe (error $ "val " ++ v ++ " not found") $ M.lookup v vals
        xstr = snd $ fromMaybe (error $ "val " ++ x ++ " not found") $ M.lookup x vals
        positive = vstr S.== xstr
        p = if b then positive else S.not positive
    in assertS p
hassert vals _ (AssertContains v b s) =
    let vstr = snd $ fromMaybe (error $ "val " ++ v ++ " not found") $ M.lookup v vals
        sstr = S.seriS (map fromChar s)
        positive = S.isInfixOf sstr vstr
        p = if b then positive else S.not positive
    in assertS p

-- inlinevals varid varval vals
--  Given the var id, it's symbolic value, and the rest of the values, return
--  a mapping from id to totally inlined values along with the length of those
--  totally inlined values (for bounds inference)
inlinevals :: ID -> (Integer, S_String) -> M.Map ID Val -> M.Map ID (Integer, S_String)
inlinevals varid varval m =
  let lookupval :: Val -> Maybe (Integer, S_String)
      lookupval (ValID x)
        | x == varid = return varval
        | otherwise = M.lookup x m >>= lookupval
      lookupval (ValLit x) = return $ (genericLength x, S.seriS (map fromChar x))
      lookupval (ValCat a b) = do
            (la, a') <- lookupval a
            (lb, b') <- lookupval b
            return $ (la + lb, a' S.++ b')
      lookupval (ValSub src off len) = do
            (_, src') <- lookupval (ValID src)
            return $ (len, S.substring src' (S.seriS off) (S.seriS len))
      vals = [(id, fromMaybe (error $ show v ++ " not found") $ lookupval v) | (id, v) <- M.toList m]
  in M.fromList $ (varid, varval) : vals

-- A hampi query.
hquery :: Hampi -> Query String
hquery (Hampi (Var vid wmin wmax) _ _ _) | wmax < wmin = return "UNSAT"
hquery (Hampi (Var vid wmin wmax) vals cfgs asserts) = do
    svar <- freevar wmin
    let svals = inlinevals vid (wmin, svar) vals
    r <- queryS $ do
        mapM_ (hassert svals cfgs) asserts
        query $ realizeS svar
    case r of
        Satisfiable v ->
          let tostr :: [Elem] -> String
              tostr = map toChar
          in return $ "{VAR(" ++ vid ++ ")=" ++ tostr v ++ "}"
        Unsatisfiable -> hquery (Hampi (Var vid (wmin + 1) wmax) vals cfgs asserts)
        _ -> return "UNKNOWN"

lookuparg :: String -> [String] -> Maybe String
lookuparg k m = 
  case dropWhile (/= k) m of
     (_:x:_) -> Just x
     _ -> Nothing

main :: IO ()
main = {-# SCC "Main" #-} do
    args <- getArgs
    (fin, to) <- case take 2 args of
             (f:i:_) | head i /= '-' -> return (f, read i)
             (f:_) -> return (f, -1)
             _ -> fail "Usage: Hampi <filename> [timeout in secs] [-d debug] [-s solver]"
    let dbg = lookuparg "-d" args
    solver <- case lookuparg "-s" args of
                 Just "yices1" -> return yices1
                 Just "yices2" -> return yices2
                 Just "stp" -> return stp
                 Just x -> fail $ "Unknown solver: " ++ x
                 Nothing -> return yices2
    input <- readFile fin
    h <- {-# SCC "Parse" #-} case runStateT parseHampi input of
            Left msg -> fail msg
            Right x -> return $ fst x
    s <- solver
    r <- timeout (1000000*to) $ runQuery (RunOptions dbg s) (hquery h)
    putStrLn (fromMaybe "TIMEOUT" r)

