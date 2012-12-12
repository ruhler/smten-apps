
module Hampi (
    ID, Elem, Assertion(..), Var(..), Val(..), Hampi(..), toChar,
    stringV, idV, concatV,
    inlineregs,
    ) where

import Debug.Trace
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

import RegEx

type Elem = Integer

instance FromChar Integer where
    fromChar = toInteger . fromEnum

toChar :: Integer -> Char
toChar = toEnum . fromInteger

data Assertion = AssertIn ID Bool ID
               | AssertContains ID Bool [Elem]

data Var = Var {
    v_id :: ID,
    v_minwidth :: Integer,
    v_maxwidth :: Integer
}

data Val = ValID ID | ValLit [Elem] | ValCat Val Val
    deriving (Show)

stringV :: String -> Val
stringV = ValLit . map fromChar

idV :: ID -> Val
idV = ValID

concatV :: [Val] -> Val
concatV = foldr1 ValCat

data Hampi = Hampi {
    h_var :: Var,
    h_vals :: Map.Map ID Val,
    h_regs :: Map.Map ID (RegEx Elem),
    h_asserts :: [Assertion]
}

inlineregs :: Hampi -> Hampi
inlineregs (Hampi var vals regs asserts) =
  let lookupreg :: RegEx Elem -> RegEx Elem
      lookupreg r
        | Star x <- r = Star (lookupreg x)
        | Concat a b <- r = Concat (lookupreg a) (lookupreg b)
        | Or a b <- r = Or (lookupreg a) (lookupreg b)
        | Fix x n <- r = Fix (lookupreg x) n
        | Variable x <- r = lookupreg $
            fromMaybe (error $ "undefined reg: " ++ x) $ Map.lookup x regs
        | otherwise = r

      regs' = Map.map lookupreg regs
  in Hampi var vals regs' asserts

