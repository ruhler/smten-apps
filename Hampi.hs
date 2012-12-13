
module Hampi (
    ID, Elem, Assertion(..), Var(..), Val(..), Hampi(..), toChar,
    stringV, idV, concatV, subV,
    ) where

import Debug.Trace
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

import SeriRegEx
import RegEx
import Elem
import Fix

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
    h_vals :: Map.Map ID Val,
    h_regs :: Map.Map ID (RegEx Elem),
    h_asserts :: [Assertion]
}

