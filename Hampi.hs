
module Hampi (
    Var(), fixedV, boundedV,
    AssertionType(..), Assertion(..), Hampi(..),
    ) where

import qualified Data.Map as Map

import CFG

data Var = Var ID Integer Integer
    deriving (Show, Eq)

fixedV :: ID -> Integer -> Var
fixedV id x = Var id x x

boundedV :: ID -> Integer -> Integer -> Var
boundedV = Var

data AssertionType = In | Contains
    deriving (Eq, Show)

data Assertion = Assertion ID Bool AssertionType CFG
    deriving (Eq, Show)

-- A HAMPI constraint.
data Hampi = Hampi {
    h_var :: Var,
    h_env :: Map.Map ID CFG,
    h_asserts :: [Assertion]
} deriving (Eq, Show)
    
