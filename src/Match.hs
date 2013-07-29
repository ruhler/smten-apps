
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}
module Match (match) where

import Smten.Prelude
import Smten.Control.Monad.State

import Smten.Data.Array
import Smten.Data.Functor
import Smten.Data.Maybe
import qualified Smten.Data.Map as Map

import SChar
import RegEx

data SubMatch = SubMatch {
    m_id :: RID,    -- ID.
    m_len :: Int,   -- Length to match.
    m_off :: Int    -- Offset in entire string to match at.
}

instance Eq SubMatch where
    (==) a b = (m_id a == m_id b)
            && (m_len a == m_len b)
            && (m_off a == m_off b)

instance Show SubMatch where
    show (SubMatch x n o) = show x ++ "." ++ show n ++ "." ++ show o

-- ((RID, Length), Offset) -> Match Result
type MatchCache = Array ((RID, Int), Int) Bool

matchidM :: MatchCache -> SubMatch -> Bool
matchidM m s@(SubMatch x n off) = m ! ((x, n), off)

matchM :: (SChar c) => MatchCache -> RegEx -> Int -> [c] -> Bool
matchM mc r off str =
  case r of
     Epsilon -> True
     Empty -> False
     Atom c -> head str == toSChar c
     Range a b -> (toSChar a <= head str) && (head str <= toSChar b)
     Concat _ a b ->
       let i = rlength a
           a' = matchM mc a off str
           b' = matchM mc b (off+i) (drop i str)
       in a' && b'
     Or _ a b ->
       let a' = matchM mc a off str
           b' = matchM mc b off str
       in a' || b'
     Variable n id -> matchidM mc (SubMatch id n off)

match :: (SChar c) => Array (RID, Int) RegEx -> RegEx -> [c] -> Bool
match regs x str =
  let (regl, regh) = bounds regs
      bnds = ((regl, 0), (regh, (length str)))
      elems = map (\(rid, off) -> matchM mc (regs ! rid) off (drop off str)) (range bnds)
      mc = listArray bnds elems
  in matchM mc x 0 str

