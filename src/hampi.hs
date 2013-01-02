
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

import System.Exit
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
import Seri.HaskellF.TH
import Seri.SMT.Query
import Seri.SMT.Yices.Yices2
import Seri.SMT.Yices.Yices1
import Seri.SMT.STP.STP
import qualified Seri.HaskellF.Lib.Prelude as S
import qualified SHampi as S
import qualified SHampi

import RegEx
import CFG
import Hampi
import Grammar
import Fix

derive_SeriT ''RegEx
derive_SeriEH ''RegEx
derive_SeriS ''RegEx ''S.RegEx

instance (S.SeriS ca fa, S.SeriS cb fb) => S.SeriS (ca, cb) (S.Tuple2__ fa fb) where
    seriS (a, b) = S.Tuple2__ (S.seriS a) (S.seriS b)

derive_SeriT ''Map
derive_SeriEH ''Map
derive_SeriS ''Map ''S.Map

-- Ignores value of the first argument. That's just to specify the type.
freevar :: (S.Element e) => e -> Integer -> Query (S.List__ e)
freevar _ = qS . S.freeElemString . S.seriS

-- Make a hampi assertion.
hassert :: (S.Element e, S.Ord e, S.Eq e) => M.Map ID (Integer, S.List__ e) -> M.Map ID CFG -> Assertion -> Query ()
hassert vals cfgs (AssertIn v b r) =
    let (vlen, vstr) = fromMaybe (error $ "val " ++ v ++ " not found") $ M.lookup v vals
        (regs, reg) = {-# SCC "FixN" #-} fixN cfgs r vlen
        reg' = {-# SCC "SeriS" #-} S.seriS reg
        regs' = {-# SCC "SeriS" #-} S.seriS regs
        b' = {-# SCC "SeriS" #-} S.seriS b
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
        sstr = S.seriS s
        positive = S.contains vstr sstr
        p = if b then positive else S.not positive
    in assertS p

-- inlinevals varid varval vals
--  Given the var id, it's symbolic value, and the rest of the values, return
--  a mapping from id to totally inlined values along with the length of those
--  totally inlined values (for bounds inference)
inlinevals :: (S.Element e) => ID -> (Integer, S.List__ e) -> M.Map ID Val -> M.Map ID (Integer, S.List__ e)
inlinevals varid varval m =
  let lookupval (ValID x)
        | x == varid = return varval
        | otherwise = M.lookup x m >>= lookupval
      lookupval (ValLit x) = return $ (genericLength x, S.toElemString (S.seriS x))
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
-- Takes an argument of Element type to specify which type to use for the
-- elemtn. The value of that argument is ignored.
hquery :: (S.Element e, S.Eq e, S.Ord e) => e -> Hampi -> Query String
hquery _ (Hampi (Var vid wmin wmax) _ _ _) | wmax < wmin = return "UNSAT"
hquery e (Hampi (Var vid wmin wmax) vals cfgs asserts) = do
    svar <- freevar e wmin
    let svals = inlinevals vid (wmin, svar) vals
    r <- queryS $ do
        mapM_ (hassert svals cfgs) asserts
        query $ realizeS' svar
    case r of
        Satisfiable v ->
          let vstr = fromMaybe (error "vstr not concrete") (S.de_seriS (S.fromElemString v))
          in return $ "{VAR(" ++ vid ++ ")=" ++ vstr ++ "}"
        Unsatisfiable -> hquery e (Hampi (Var vid (wmin + 1) wmax) vals cfgs asserts)
        _ -> return "UNKNOWN"

data ElemType = ET_Integer | ET_Bit
    deriving (Eq, Show)

lookuparg :: String -> [String] -> Maybe String
lookuparg k m = 
  case dropWhile (/= k) m of
     (_:x:_) -> Just x
     _ -> Nothing

usage :: String
usage = "Usage: Hampi <filename> [timeout in secs] [-d debug] [-s yices1 | yices2 | stp] [-e Integer | Bit]"

main :: IO ()
main = {-# SCC "Main" #-} do
    args <- getArgs
    (fin, to) <- case take 2 args of
             ("--help":_) -> putStrLn usage >> exitSuccess
             (f:i:_) | head i /= '-' -> return (f, read i)
             (f:_) -> return (f, -1)
             _ -> fail usage
    let dbg = lookuparg "-d" args
    solver <- case lookuparg "-s" args of
                 Just "yices1" -> return yices1
                 Just "yices2" -> return yices2
                 Just "stp" -> return stp
                 Just x -> fail $ "Unknown solver: " ++ x ++ ".\n" ++ usage
                 Nothing -> return yices2
    elemtype <- case lookuparg "-e" args of
                 Just "Integer" -> return ET_Integer
                 Just "Bit" -> return ET_Bit
                 Just x -> fail $ "Unknown elem type: " ++ x ++ ".\n" ++ usage
                 Nothing -> return ET_Bit

    input <- readFile fin
    h <- {-# SCC "Parse" #-} case runStateT parseHampi input of
            Left msg -> fail msg
            Right x -> return $ fst x
    s <- solver
    let hq = case elemtype of
                ET_Bit -> hquery S.bitElem h
                ET_Integer -> hquery S.integerElem h
    r <- timeout (1000000*to) $ runQuery (RunOptions dbg s) hq
    putStrLn (fromMaybe "TIMEOUT" r)

