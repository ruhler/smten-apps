
module Imports (
    getArgs, exitSuccess, readFile, parseHampi, timeout, read,
    IO, Char, Integer, Maybe, Either, Hampi,
    ) where

import System.Environment (getArgs)
import qualified System.Timeout as S
import System.Exit

import Control.Monad.State

import Hampi
import qualified Grammar as S

timeout :: Integer -> IO a -> IO (Maybe a)
timeout = S.timeout . fromInteger
    
parseHampi :: String -> Either String Hampi
parseHampi s = case runStateT S.parseHampi s of
                 Left msg -> Left msg
                 Right v -> Right (fst v)

