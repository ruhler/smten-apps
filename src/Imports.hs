
{-# LANGUAGE TemplateHaskell #-}

module Imports (
    getArgsP, exitSuccessP, readFileP, parseHampiP, timeoutP, readP,
    ) where

import System.Environment (getArgs)
import System.Timeout
import System.Exit

import Control.Monad.State

import Smten.Type
import Smten.ExpH
import Smten.Prim

import CFG
import Hampi
import Grammar

derive_SmtenT "CFG" ''CFG
derive_SmtenEH "CFG" ''CFG
derive_SmtenT "Hampi" ''Val
derive_SmtenEH "Hampi" ''Val
derive_SmtenT "Hampi" ''Assertion
derive_SmtenEH "Hampi" ''Assertion
derive_SmtenT "Hampi" ''Var
derive_SmtenEH "Hampi" ''Var
derive_SmtenT "Hampi" ''Hampi
derive_SmtenEH "Hampi" ''Hampi

getArgsP :: Prim
getArgsP = nullaryP "Imports.getArgs" getArgs

exitSuccessP :: Prim
exitSuccessP = nullaryP "Imports.exitSuccess" (exitSuccess :: IO ExpH)

readFileP :: Prim
readFileP = unaryP "Imports.readFile" readFile

timeoutP :: Prim
timeoutP =
  let f :: Integer -> IO ExpH -> IO (Maybe ExpH)
      f i q = timeout (fromInteger i) q
  in binaryP "Imports.timeout" f
    
parseHampiP :: Prim
parseHampiP =
  let f :: String -> Either String Hampi
      f s = case runStateT parseHampi s of
                Left msg -> Left msg
                Right v -> Right (fst v)
  in unaryP "Imports.parseHampi" f

readP :: Prim
readP = unaryP "Imports.unary" (read :: String -> Integer)

