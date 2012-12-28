
module Elem where

type Elem = Integer
type ElemString = [Elem]

fromChar :: Char -> Elem
fromChar = toInteger . fromEnum

toChar :: Elem -> Char
toChar = toEnum . fromInteger

