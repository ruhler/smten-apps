
module Ppr2 (pretty) where

import Smten.Prelude

import Syntax

-- | Pretty print a type.
pprT :: Type -> String
pprT VoidT = "void"
pprT BitT = "bit"
pprT (ArrT ty w) = pprT ty ++ "[??]"    -- TODO: print the array width
pprT IntT = "int"
pprT (FunT {}) = error $ "pprT: no way to print function type"
pprT (StructT nm) = nm
pprT UnknownT = error $ "pprT: unknown type"

-- Separate the given strings with commas.
commas :: [String] -> String
commas [] = []
commas [x] = x
commas (x:xs) = x ++ "," ++ commas xs

-- | Pretty print a value.
pprV :: Value -> String
pprV (ArrayV vs) = "{" ++ commas (map pprV vs) ++ "}"
pprV (BitV True) = "true"
pprV (BitV False) = "false"
pprV (IntV i) = show i
pprV (FunV {}) = error "pprV: no way to print function value"
pprV VoidV = error "pprV: no way to print void value"
pprV (PointerV Null) = "null"
pprV (PointerV {}) = error "pprV: no way to print non-null pointer value"

type Prec = Int
p_min = 0
p_add = p_min
p_sub = p_add
p_mul = p_add + 1
p_div = p_mul

-- | parens p p_op x
-- Parenthesize the given expression if p_op is lower than p.
parens :: Prec -> Prec -> String -> String
parens p p_op x
  | p_op < p = "(" ++ x ++ ")"
  | otherwise = x

-- | Pretty print an expression.
-- If the precedence of the operator is lower than given p, then the
-- expression must be parenthesized.
pprE :: Prec -> Expr -> String
pprE p e =
  case e of
    ValE v -> pprV v
    AddE a b -> parens p p_add $ pprE p_add a ++ " + " ++ pprE p_add b
    SubE a b -> parens p p_sub $ pprE p_sub a ++ " - " ++ pprE (p_sub+1) b
    MulE a b -> parens p p_mul $ pprE p_mul a ++ " * " ++ pprE p_mul b
    DivE a b -> parens p p_div $ pprE p_div a ++ " / " ++ pprE (p_div+1) b
    

class Pretty a where
   pretty :: a -> String

instance Pretty Type where
   pretty = pprT

instance Pretty Value where
   pretty = pprV

instance Pretty Expr where
   pretty = pprE 0
