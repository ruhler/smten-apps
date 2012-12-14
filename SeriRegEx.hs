
-- This file shared by Haskell and Seri.
-- So it should be restricted to Seri syntax and features.
module SeriRegEx where

import Prelude

data RegEx c =
             Epsilon        -- matches ""
           | Empty          -- never matches
           | Atom c
           | Range c c
           | Concat Integer (RegEx c) (RegEx c)
           | Or Integer (RegEx c) (RegEx c)
   deriving(Eq)

-- The length a string much be to match against the given regular expression.
rlength :: RegEx c -> Integer
rlength Epsilon = 0
rlength Empty = 1   -- not strictly correct, what should go here?
rlength (Atom _) = 1
rlength (Range _ _) = 1
rlength (Concat n _ _) = n
rlength (Or n _ _) = n

