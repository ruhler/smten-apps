
module Elem (Elem, fromChar, toChar) where

import Data.Functor((<$>))

import Seri.Bit
import Seri.Type
import Seri.ExpH

import SeriRegEx

-- Changes to Elem must also be updated where S_Elem is specified in hampi.hs.
type Elem = Integer
--type Elem = BitElem

class Element a where
    fromChar :: Char -> a
    toChar :: a -> Char

instance Element Integer where
    fromChar = toInteger . fromEnum
    toChar = toEnum . fromInteger

newtype BitElem = BitElem Bit
    deriving (Show, Eq)

instance SeriT BitElem where
    seriT _ = bitT 8

instance SeriEH BitElem where
    seriEH (BitElem b) = bitEH b
    de_seriEH x = BitElem <$> de_bitEH x

instance Element BitElem where
    fromChar c = BitElem $ bv_make 8 (fromChar c)
    toChar (BitElem b) = toChar (bv_value b)

