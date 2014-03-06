
module Ppr (pretty) where

import Smten.Prelude
import RegEx

-- Pretty print a regular expression
pretty :: RegEx -> String
pretty = prettyP p_max

p_max = p_reg
p_reg = 3
p_branch = 2
p_piece = 1
p_atom = 0

-- Pretty print a regular expression with given precedence.
prettyP :: Int -> RegEx -> String
prettyP _ Epsilon = "()"
prettyP _ Empty = "∅"
prettyP _ (Atom c) = [c]
prettyP _ (Range l h) = ['[', l, '-', h, ']']
prettyP p (Concat a b) = parens (p < p_branch) $
    prettyP p_branch a ++ prettyP p_branch b
prettyP p (Or a b) = parens (p < p_reg) $
    prettyP p_reg a ++ " | " ++ prettyP p_reg b
prettyP p (Star a) = parens (p < p_piece) $
    prettyP p_atom a ++ "*"

parens :: Bool -> String -> String
parens True x = "(" ++ x ++ ")"
parens False x = x


