
module RegEx (
    RegEx,
    epsilonR, emptyR, atomR, rangeR, concatR, orR, starR, plusR, optionR,
    match,
    pretty,
    ) where

import Smten.Prelude

data RegEx =
    Epsilon
  | Empty
  | Atom Char
  | Range Char Char
  | Concat RegEx RegEx
  | Or RegEx RegEx
  | Star RegEx
    deriving (Eq)

epsilonR :: RegEx
epsilonR = Epsilon

emptyR :: RegEx
emptyR = Empty

atomR :: Char -> RegEx
atomR = Atom

rangeR :: Char -> Char -> RegEx
rangeR a b =
  case compare a b of
     LT -> Range a b
     EQ -> Atom a
     GT -> Empty

concatR :: RegEx -> RegEx -> RegEx
concatR Empty _ = Empty
concatR _ Empty = Empty
concatR Epsilon a = a
concatR a Epsilon = a
concatR a b = Concat a b

orR :: RegEx -> RegEx -> RegEx
orR Empty a = a
orR a Empty = a
orR a b = Or a b

starR :: RegEx -> RegEx
starR Empty = Empty
starR Epsilon = Epsilon
starR (Star a) = Star a
starR a = Star a

plusR :: RegEx -> RegEx
plusR x = concatR x (starR x)

optionR :: RegEx -> RegEx
optionR x = orR x epsilonR 

pretty :: RegEx -> String
pretty r =
    case r of
        Epsilon -> "ε"
        Empty -> "∅"
        Atom a -> show a
        Range a b -> "[" ++ show a ++ "-" ++ show b ++ "]"
        Concat a b -> pretty a ++ pretty b
        Or a b -> "(" ++ pretty a ++ " | " ++ pretty b ++ ")"
        Star x -> "(" ++ pretty x ++ ")*"

match :: RegEx -> String -> Bool
match r str = 
  case r of
    Epsilon -> null str
    Empty -> False
    Atom c -> str == [c]
    Range a b ->
      case str of
        [x] -> (a <= x && x <= b)
        _ -> False
    Concat a b ->
      let f i = case splitAt i str of
                   (astr, bstr) -> match a astr && match b bstr
      in any f [0..length str]
    Or a b -> match a str || match b str
    Star x ->
      let f i = case splitAt i str of
                   (astr, bstr) -> match x astr && match r bstr
      in null str || any f [1..length str]

