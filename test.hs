
{-# LANGUAGE TemplateHaskell #-}

import Seri.Type
import Seri.Exp
import Seri.ExpH
import qualified Seri.HaskellF.Symbolic as S
import Seri.HaskellF.Query
import Seri.SMT.Query
import Seri.SMT.Yices.Yices2
import qualified Seri.HaskellF.Lib.Prelude as S
import qualified SeriGen as S

import RegEx

derive_SeriT ''RegEx
derive_SeriEH ''RegEx

instance FromChar Integer where
    fromChar = toInteger . fromEnum

abstar :: RegEx Integer
abstar = starR (stringR "ab")

s_abstar :: S.RegEx S.Integer
s_abstar = S.seriS abstar

str :: [Integer]
str = map fromChar "abab"

s_str :: S.List__ S.Integer
s_str = S.seriS str

result :: Maybe Bool
result = S.de_seriS (S.match s_abstar s_str)

freevar :: Integer -> Query (S.List__ S.Integer)
freevar = qS . S.frees . S.seriS

q :: Query (Answer [Integer])
q = do
    var <- freevar 6
    assertS $ S.match s_abstar var
    query $ realizeS var

main :: IO ()
main = do
    y <- yices2
    r <- runQuery (RunOptions (Just "tesths.dbg") y) q
    putStrLn $ show r

