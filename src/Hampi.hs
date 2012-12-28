
module Hampi (
    ID, Assertion(..), Var(..), Val(..), Hampi(..),
    stringV, idV, concatV, subV,
    ) where

import Debug.Trace
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

import RegEx
import CFG

data Assertion = AssertIn ID Bool ID
               | AssertContains ID Bool String
               | AssertEquals ID Bool ID

data Var = Var {
    v_id :: ID,
    v_minwidth :: Integer,
    v_maxwidth :: Integer
}

data Val = ValID ID
         | ValLit String
         | ValCat Val Val
         | ValSub {
             vs_source :: ID,
             vs_offset :: Integer,
             vs_length :: Integer
           }
    deriving (Show)

stringV :: String -> Val
stringV = ValLit

idV :: ID -> Val
idV = ValID

subV :: ID -> Integer -> Integer -> Val
subV = ValSub

concatV :: [Val] -> Val
concatV = foldr1 ValCat

data Hampi = Hampi {
    h_var :: Var,
    h_vals :: Map.Map ID Val,
    h_cfgs :: Map.Map ID CFG,
    h_asserts :: [Assertion]
}

