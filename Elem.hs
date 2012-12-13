
module Elem (Elem, toChar) where

import SeriRegEx

type Elem = Integer

instance FromChar Integer where
    fromChar = toInteger . fromEnum

toChar :: Elem -> Char
toChar = toEnum . fromInteger

