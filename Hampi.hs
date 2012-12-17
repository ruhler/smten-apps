
module Hampi (
    ID, Elem, Assertion(..), Var(..), Val(..), Hampi(..), toChar,
    stringV, idV, concatV, subV,
    ) where

import Debug.Trace
import Map
import Data.Maybe (fromMaybe)

import Elem
import CFG

data Assertion = AssertIn ID Bool ID
               | AssertContains ID Bool [Elem]
               | AssertEquals ID Bool ID

data Var = Var {
    v_id :: ID,
    v_minwidth :: Integer,
    v_maxwidth :: Integer
}

data Val = ValID ID
         | ValLit [Elem]
         | ValCat Val Val
         | ValSub {
             vs_source :: ID,
             vs_offset :: Integer,
             vs_length :: Integer
           }
    deriving (Show)

stringV :: String -> Val
stringV = ValLit . map fromChar

idV :: ID -> Val
idV = ValID

subV :: ID -> Integer -> Integer -> Val
subV = ValSub

concatV :: [Val] -> Val
concatV = foldr1 ValCat

data Hampi = Hampi {
    h_var :: Var,
    h_vals :: Map ID Val,
    h_cfgs :: Map ID CFG,
    h_asserts :: [Assertion]
}

