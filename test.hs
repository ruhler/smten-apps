
{-# LANGUAGE TemplateHaskell #-}

import qualified Data.Map as Map
import Data.Maybe (fromJust)
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

import RegEx

derive_SeriT ''RegEx
derive_SeriEH ''RegEx

instance FromChar Integer where
    fromChar = toInteger . fromEnum

abstar :: RegEx Integer
abstar = starR (stringR "ab")

freevar :: Integer -> Query S_String
freevar = qS . S.frees . S.seriS

type ID = String
type Elem = Integer
type S_Elem = S.Integer
type S_String = S.List__ S_Elem

data Assertion = Assert ID Bool ID

-- Make a hampi assertion.
hassert :: Map.Map ID S_String -> Map.Map ID (RegEx Elem) -> Assertion -> Query ()
hassert vals regs (Assert v b r) =
    let vstr = fromJust $ Map.lookup v vals
        rreg = S.seriS . fromJust $ Map.lookup r regs
        positive = S.match rreg vstr
        p = if b then positive else S.not positive
    in assertS p

data Val = ValID ID | ValLit [Elem] | ValCat Val Val

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
      vals = [(id, fromJust $ lookupval v) | (id, v) <- Map.toList m]
  in Map.fromList $ (varid, varval) : vals

data Var = Var {
    v_id :: ID,
    v_width :: Integer
}

data Hampi = Hampi {
    h_var :: Var,
    h_vals :: Map.Map ID Val,
    h_regs :: Map.Map ID (RegEx Elem),
    h_asserts :: [Assertion]
}

-- A hampi query.
hquery :: Hampi -> Query (Answer [Elem])
hquery (Hampi (Var vid vwidth) vals regs asserts) = do
    svar <- freevar vwidth
    let svals = inlinevals vid svar vals
    mapM_ (hassert svals regs) asserts
    query $ realizeS svar

htest :: Hampi
htest = Hampi {
    h_var = Var "str" 6,
    h_vals = Map.empty,
    h_regs = Map.singleton "abstar" abstar,
    h_asserts = [Assert "str" True "abstar"]
}

main :: IO ()
main = do
    y <- yices2
    r <- runQuery (RunOptions (Just "tesths.dbg") y) (hquery htest)
    putStrLn $ show r

