
-- This file shared by Haskell and Seri.
-- So it should be restricted to Seri syntax and features.
module SeriRegEx where

import Prelude

type Elem = Integer

data RegEx =
           Epsilon        -- matches ""
         | Empty          -- never matches
         | Atom Elem
         | Range Elem Elem
         | Concat Integer RegEx RegEx
         | Or Integer RegEx RegEx
   deriving(Eq)

-- The length a string much be to match against the given regular expression.
rlength :: RegEx -> Integer
rlength Epsilon = 0
rlength Empty = 1   -- not strictly correct, what should go here?
rlength (Atom _) = 1
rlength (Range _ _) = 1
rlength (Concat n _ _) = n
rlength (Or n _ _) = n

