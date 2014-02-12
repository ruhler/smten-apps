
module Hampi where

import Smten.Prelude
import qualified Smten.Data.Map as Map
import CFG
import Fix

data Assertion = AssertIn ID Bool ID
               | AssertContains ID Bool String
               | AssertEquals ID Bool ID

data Var = Var {
    v_id :: ID,
    v_minwidth :: Int,
    v_maxwidth :: Int
}

data Val = ValID ID
         | ValLit String
         | ValCat Val Val
         | ValSub {
             vs_source :: ID,
             vs_offset :: Int,
             vs_length :: Int
           }

instance Show Val where
    show (ValID x) = "@" ++ show x
    show (ValLit str) = show str
    show (ValCat a b) = show a ++ " ++ " ++ show b
    show (ValSub a b c) = "@" ++ show a ++ "[" ++ show b ++ ":" ++ show c ++ "]"

stringV :: String -> Val
stringV = ValLit

idV :: ID -> Val
idV = ValID

subV :: ID -> Int -> Int -> Val
subV = ValSub

concatV :: [Val] -> Val
concatV = foldr1 ValCat

data Hampi = Hampi {
    h_var :: Var,
    h_vals :: [(ID, Val)],
    h_cfgs :: Map.Map ID CFG,
    h_asserts :: [Assertion]
}

