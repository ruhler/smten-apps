
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

import Smten.Type
import Smten.Exp
import Smten.ExpH
import Smten.Ppr
import Smten.HaskellF.HaskellF
import Smten.HaskellF.SMT
import Smten.HaskellF.TH
import Smten.SMT.Symbolic
import Smten.SMT.SMT
import Smten.SMT.Yices.Yices2
import Smten.SMT.Yices.Yices1
import Smten.SMT.STP.STP
import qualified Smten.HaskellF.Lib.Prelude as S
import qualified SHampi as S
import qualified SHampi

import RegEx
import CFG
import Hampi
import Grammar
import Fix

derive_SmtenT ''RegEx
derive_SmtenEH ''RegEx
derive_SmtenHF ''RegEx ''S.RegEx
derive_SmtenT ''Map
derive_SmtenEH ''Map
derive_SmtenHF ''Map ''S.Map

-- Ignores value of the first argument. That's just to specify the type.
freevar :: (S.SChar c) => c -> Integer -> Symbolic (S.List__ c)
freevar _ = symbolicHF . S.freeSCharString . smtenHF

-- Make a hampi assertion.
hassert :: (S.SChar c) => M.Map ID (Integer, S.List__ c) -> M.Map ID CFG -> Assertion -> Symbolic ()
hassert vals cfgs (AssertIn v b r) =
    let (vlen, vstr) = fromMaybe (error $ "val " ++ v ++ " not found") $ M.lookup v vals
        (regs, reg) = {-# SCC "FixN" #-} fixN cfgs r vlen
        reg' = {-# SCC "SmtenHF" #-} smtenHF reg
        regs' = {-# SCC "SmtenHF" #-} smtenHF regs
        b' = {-# SCC "SmtenHF" #-} smtenHF b
        p = {-# SCC "AssertIn" #-} S.assertIn regs' vstr b' reg'
    in assertHF p
hassert vals _ (AssertEquals v b x) =
    let vstr = snd $ fromMaybe (error $ "val " ++ v ++ " not found") $ M.lookup v vals
        xstr = snd $ fromMaybe (error $ "val " ++ x ++ " not found") $ M.lookup x vals
        positive = vstr S.== xstr
        p = if b then positive else S.not positive
    in assertHF p
hassert vals _ (AssertContains v b s) =
    let vstr = snd $ fromMaybe (error $ "val " ++ v ++ " not found") $ M.lookup v vals
        sstr = smtenHF s
        positive = S.contains vstr sstr
        p = if b then positive else S.not positive
    in assertHF p

-- inlinevals varid varval vals
--  Given the var id, it's symbolic value, and the rest of the values, return
--  a mapping from id to totally inlined values along with the length of those
--  totally inlined values (for bounds inference)
inlinevals :: (S.SChar c) => ID -> (Integer, S.List__ c) -> M.Map ID Val -> M.Map ID (Integer, S.List__ c)
inlinevals varid varval m =
  let lookupval (ValID x)
        | x == varid = return varval
        | otherwise = M.lookup x m >>= lookupval
      lookupval (ValLit x) = return $ (genericLength x, S.toSCharString (smtenHF x))
      lookupval (ValCat a b) = do
            (la, a') <- lookupval a
            (lb, b') <- lookupval b
            return $ (la + lb, a' S.++ b')
      lookupval (ValSub src off len) = do
            (_, src') <- lookupval (ValID src)
            return $ (len, S.substring src' (smtenHF off) (smtenHF len))
      vals = [(id, fromMaybe (error $ show v ++ " not found") $ lookupval v) | (id, v) <- M.toList m]
  in M.fromList $ (varid, varval) : vals

-- A hampi query.
-- Takes an argument of SChar type to specify which type to use for the
-- elemtn. The value of that argument is ignored.
hquery :: (S.SChar c) => c -> Hampi -> SMT String
hquery _ (Hampi (Var vid wmin wmax) _ _ _) | wmax < wmin = return "UNSAT"
hquery e (Hampi (Var vid wmin wmax) vals cfgs asserts) = do
    r <- queryHF $ do
        svar <- freevar e wmin
        let svals = inlinevals vid (wmin, svar) vals
        mapM_ (hassert svals cfgs) asserts
        return svar
    case r of
        Just v ->
          let vstr = fromMaybe (error "vstr not concrete") (de_smtenHF (S.fromSCharString v))
          in return $ "{VAR(" ++ vid ++ ")=" ++ vstr ++ "}"
        Nothing -> hquery e (Hampi (Var vid (wmin + 1) wmax) vals cfgs asserts)

data SCharType = SChar_Integer | SChar_Bit
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
                 Just "Integer" -> return SChar_Integer
                 Just "Bit" -> return SChar_Bit
                 Just x -> fail $ "Unknown elem type: " ++ x ++ ".\n" ++ usage
                 Nothing -> return SChar_Bit

    input <- readFile fin
    h <- {-# SCC "Parse" #-} case runStateT parseHampi input of
            Left msg -> fail msg
            Right x -> return $ fst x
    s <- solver
    let hq = case elemtype of
                SChar_Bit -> hquery S.bitSChar h
                SChar_Integer -> hquery S.integerSChar h
    r <- timeout (1000000*to) $ runSMT (RunOptions dbg s) hq
    putStrLn (fromMaybe "TIMEOUT" r)

