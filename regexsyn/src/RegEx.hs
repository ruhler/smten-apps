
module RegEx (
    RegEx,
    epsilonR, emptyR, atomR, rangeR, concatR, orR, starR,
    match,
    pretty, prettyc
    ) where

import Smten.Prelude

data RegEx a =
    Epsilon
  | Empty
  | Atom a
  | Range a a
  | Concat (RegEx a) (RegEx a)
  | Or (RegEx a) (RegEx a)
  | Star (RegEx a)
    deriving (Eq)

epsilonR :: RegEx a
epsilonR = Epsilon

emptyR :: RegEx a
emptyR = Empty

atomR :: a -> RegEx a
atomR = Atom

rangeR :: (Ord a) => a -> a -> RegEx a
rangeR a b =
  case compare a b of
     LT -> Range a b
     EQ -> Atom a
     GT -> Empty

concatR :: RegEx a -> RegEx a -> RegEx a
concatR Empty _ = Empty
concatR _ Empty = Empty
concatR Epsilon a = a
concatR a Epsilon = a
concatR a b = Concat a b

orR :: RegEx a -> RegEx a -> RegEx a
orR Empty a = a
orR a Empty = a
orR a b = Or a b

starR :: RegEx a -> RegEx a
starR Empty = Empty
starR Epsilon = Epsilon
starR (Star a) = Star a
starR a = Star a

pretty :: (a -> String) -> RegEx a -> String
pretty f r =
    case r of
        Epsilon -> "ε"
        Empty -> "∅"
        Atom a -> f a
        Range a b -> "[" ++ f a ++ "-" ++ f b ++ "]"
        Concat a b -> pretty f a ++ pretty f b
        Or a b -> "(" ++ pretty f a ++ " | " ++ pretty f b ++ ")"
        Star x -> "(" ++ pretty f x ++ ")*"

prettyc :: RegEx Char -> String
prettyc = pretty show

match :: (Ord a) => RegEx a -> [a] -> Bool
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

