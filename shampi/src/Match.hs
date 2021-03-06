
module Match (match) where

import Smten.Prelude
import Smten.Data.Array

import SChar
import RegEx

data SubMatch = SubMatch {
    _m_id :: RID,    -- ID.
    _m_len :: Int,   -- Length to match.
    _m_off :: Int    -- Offset in entire string to match at.
} deriving (Eq)

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

