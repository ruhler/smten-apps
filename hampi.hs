
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

import System.Environment
import System.Timeout
import Control.Monad.State

import Debug.Trace

import Map
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
import qualified SeriGen as S

import Elem
import SeriRegEx
import SeriCFG
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
    

--type S_2 = S.N__2p0 (S.N__2p1 S.N__0)
--type S_4 = S.N__TIMES S_2 S_2
--type S_8 = S.N__TIMES S_4 S_2
--type S_Elem = S.Bit S_8
type S_Elem = S.Integer
type S_String = S.List__ S_Elem

freevar :: Integer -> Query S_String
freevar = qS . S.frees . S.seriS

-- Make a hampi assertion.
hassert :: Map ID (Integer, S_String) -> Map ID CFG -> Assertion -> Query ()
hassert vals cfgs (AssertIn v b r) =
    let (vlen, vstr) = fromMaybe (error $ "val " ++ v ++ " not found") $ map_lookup v vals
        (regs, reg) = {-# SCC "fixN" #-} fixN cfgs r vlen
        reg' = {-# SCC "seriS_reg" #-} S.seriS reg
        regs' = {-# SCC "seriS_regs" #-} S.seriS regs
        b' = S.seriS b
        p = {-# SCC "assertIn" #-} S.assertIn regs' vstr b' reg'
    in assertS p
hassert vals _ (AssertEquals v b x) =
    let vstr = snd $ fromMaybe (error $ "val " ++ v ++ " not found") $ map_lookup v vals
        xstr = snd $ fromMaybe (error $ "val " ++ x ++ " not found") $ map_lookup x vals
        positive = vstr S.== xstr
        p = if b then positive else S.not positive
    in assertS p
hassert vals _ (AssertContains v b s) =
    let vstr = snd $ fromMaybe (error $ "val " ++ v ++ " not found") $ map_lookup v vals
        sstr = S.seriS s
        positive = S.isInfixOf sstr vstr
        p = if b then positive else S.not positive
    in assertS p

-- inlinevals varid varval vals
--  Given the var id, it's symbolic value, and the rest of the values, return
--  a mapping from id to totally inlined values along with the length of those
--  totally inlined values (for bounds inference)
inlinevals :: ID -> (Integer, S_String) -> Map ID Val -> Map ID (Integer, S_String)
inlinevals varid varval m =
  let lookupval :: Val -> Maybe (Integer, S_String)
      lookupval (ValID x)
        | x == varid = return varval
        | otherwise = map_lookup x m >>= lookupval
      lookupval (ValLit x) = return $ (genericLength x, S.seriS x)
      lookupval (ValCat a b) = do
            (la, a') <- lookupval a
            (lb, b') <- lookupval b
            return $ (la + lb, a' S.++ b')
      lookupval (ValSub src off len) = do
            (_, src') <- lookupval (ValID src)
            return $ (len, S.substring src' (S.seriS off) (S.seriS len))
      vals = [(id, fromMaybe (error $ show v ++ " not found") $ lookupval v) | (id, v) <- map_toList m]
  in map_fromList $ (varid, varval) : vals

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

main :: IO ()
main = do
    args <- getArgs
    (fin, to, dbg) <- case args of
             [f] -> return (f, -1, Nothing)
             [f, "-d", dbg] -> return (f, -1, Just dbg)
             [f, i] -> return (f, read i, Nothing)
             [f, i, "-d", dbg] -> return (f, read i, Just dbg)
             _ -> fail "Usage: Hampi <filename> [timeout in secs]"
    input <- readFile fin
    h <- case runStateT parseHampi input of
            Left msg -> fail msg
            Right x -> return $ fst x
    y <- yices2
    r <- timeout (1000000*to) $ runQuery (RunOptions dbg y) (hquery h)
    putStrLn (fromMaybe "TIMEOUT" r)

