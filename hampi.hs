
{-# LANGUAGE TemplateHaskell #-}

import System.Environment
import System.Timeout
import Control.Monad.State

import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Functor ((<$>))

import Seri.Type
import Seri.Exp
import Seri.ExpH
import qualified Seri.HaskellF.Symbolic as S
import Seri.HaskellF.Query
import Seri.SMT.Query
import Seri.SMT.Yices.Yices2
import qualified Seri.HaskellF.Lib.Prelude as S
import qualified SeriGen as S

import SeriRegEx
import RegEx
import Hampi
import Grammar

derive_SeriT ''RegEx
derive_SeriEH ''RegEx

type S_Elem = S.Integer
type S_String = S.List__ S_Elem

freevar :: Integer -> Query S_String
freevar = qS . S.frees . S.seriS

-- Make a hampi assertion.
hassert :: Map.Map ID S_String -> Map.Map ID (RegEx Elem) -> Assertion -> Query ()
hassert vals regs (AssertIn v b r) =
    let vstr = fromMaybe (error $ "val " ++ v ++ " not found") $ Map.lookup v vals
        rreg = S.seriS . fromMaybe (error $ "reg " ++ r ++ " not found") $ Map.lookup r regs
        positive = S.match rreg vstr
        p = if b then positive else S.not positive
    in assertS p
hassert vals regs (AssertEquals v b x) =
    let vstr = fromMaybe (error $ "val " ++ v ++ " not found") $ Map.lookup v vals
        xstr = fromMaybe (error $ "val " ++ x ++ " not found") $ Map.lookup x vals
        positive = vstr S.== xstr
        p = if b then positive else S.not positive
    in assertS p
hassert vals regs (AssertContains v b s) =
    let vstr = fromMaybe (error $ "val " ++ v ++ " not found") $ Map.lookup v vals
        sstr = S.seriS s
        positive = S.isInfixOf sstr vstr
        p = if b then positive else S.not positive
    in assertS p

-- inlinevals varid varval vals
--  Given the var id, it's symbolic value, and the rest of the values, return
--  a mapping from id to totally inlined values.
inlinevals :: ID -> S_String -> Map.Map ID Val -> Map.Map ID S_String
inlinevals varid varval m =
  let lookupval :: Val -> Maybe S_String
      lookupval (ValID x)
        | x == varid = return varval
        | otherwise = Map.lookup x m >>= lookupval
      lookupval (ValLit x) = return $ S.seriS x
      lookupval (ValCat a b) = do
            a' <- lookupval a
            b' <- lookupval b
            return $ a' S.++ b'
      vals = [(id, fromMaybe (error $ show v ++ " not found") $ lookupval v) | (id, v) <- Map.toList m]
  in Map.fromList $ (varid, varval) : vals

-- A hampi query.
hquery :: Hampi -> Query String
hquery (Hampi (Var vid wmin wmax) _ _ _) | wmax < wmin = return "UNSAT"
hquery (Hampi (Var vid wmin wmax) vals regs asserts) = do
    svar <- freevar wmin
    let svals = inlinevals vid svar vals
    r <- queryS $ do
        mapM_ (hassert svals regs) asserts
        query $ realizeS svar
    case r of
        Satisfiable v ->
          let tostr :: [Elem] -> String
              tostr = map toChar
          in return $ "{VAR(" ++ vid ++ ")=" ++ tostr v ++ "}"
        Unsatisfiable -> hquery (Hampi (Var vid (wmin + 1) wmax) vals regs asserts)
        _ -> return "UNKNOWN"

main :: IO ()
main = do
    args <- getArgs
    (fin, to) <- case args of
             [f] -> return (f, -1)
             [f, i] -> return (f, read i)
             _ -> fail "Usage: Hampi <filename> [timeout in secs]"
    input <- readFile fin
    h <- case runStateT parseHampi input of
            Left msg -> fail msg
            Right x -> return $ fst x
    y <- yices2
    r <- timeout (1000000*to) $ runQuery (RunOptions (Just $ fin ++ ".dbg") y) (hquery (inlineregs h))
    putStrLn (fromMaybe "TIMEOUT" r)

