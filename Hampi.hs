
module Hampi (
    ID, Elem, Assertion(..), Var(..), Val(..), Hampi(..), toChar,
    stringV, idV, concatV,
    ) where

import qualified Data.Map as Map

import RegEx

type ID = String
type Elem = Integer

instance FromChar Integer where
    fromChar = toInteger . fromEnum

toChar :: Integer -> Char
toChar = toEnum . fromInteger

data Assertion = Assert ID Bool ID

data Var = Var {
    v_id :: ID,
    v_width :: Integer
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

