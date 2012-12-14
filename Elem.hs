
module Elem (Elem, fromChar, toChar) where

import SeriRegEx

type Elem = Integer

fromChar :: Char -> Elem
fromChar = toInteger . fromEnum

toChar :: Elem -> Char
toChar = toEnum . fromInteger

