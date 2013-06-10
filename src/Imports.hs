
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Imports (
    getArgs, exitSuccess, readFile, parseHampi, read,
    ) where

import qualified Prelude as P
import Prelude hiding (read, readFile)
import qualified System.Environment as P (getArgs)
import qualified System.Timeout as P
import qualified System.Exit as P

import Control.Monad.State

import Smten.Runtime.SmtenHS (Haskelly(..), SmtenHS0, SmtenHS1)
import qualified Smten.Runtime.SmtenHS as S
import qualified Smten.Runtime.IO as S

import Hampi
import qualified Grammar as P

getArgs :: (Haskelly (IO [String]) x) => x
getArgs = {-# SCC "IMPORT_getArgs" #-} frhs P.getArgs

--timeout :: (Haskelly Integer i, SmtenHS0 i,
--            Haskelly (IO a) io, SmtenHS ioa, SmtenHS0 ioma,
--            Haskelly (IO (Maybe a)) ioma) => i -> io a -> io (m a)
--timeout i x = frhs (P.timeout (fromInteger (tohs i :: Maybe a)) (tohs x))

exitSuccess :: (Haskelly () u, SmtenHS0 u) => S.IO u
exitSuccess = {-# SCC "IMPORT_exitSuccess" #-} frhs (P.exitSuccess :: P.IO ())

readFile :: (Haskelly String string, SmtenHS0 string) => string -> S.IO string
readFile = {-# SCC "IMPORT_readFile" #-} frhs P.readFile

read :: (Haskelly String string, SmtenHS0 string) => string -> S.Integer
read = {-# SCC "IMPORT_read" #-} frhs (P.read :: String -> Integer)
    
parseHampi :: (Haskelly String string,
               Haskelly (Either String Hampi) x) => string -> x
parseHampi s = {-# SCC "IMPORT_parseHampi" #-}
    frhs $ case runStateT P.parseHampi (tohs s) of
               Left msg -> Left msg
               Right v -> Right (fst v)

