
module Elem (Elem, fromChar, toChar) where

import SeriRegEx

type Elem = Integer

instance FromChar Integer where
    fromChar = toInteger . fromEnum

toChar :: Elem -> Char
toChar = toEnum . fromInteger

