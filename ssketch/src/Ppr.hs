
-- | Pretty printer for Sketch programs
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Ppr (pretty) where

import Smten.Prelude

import Program
import Syntax

-- | Pretty print a type.
pprT :: Type -> String
pprT VoidT = "void"
pprT BitT = "bit"
pprT (ArrT ty w) = pprT ty ++ "[" ++ pretty w ++ "]"
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
pprV (BitV True) = "1"
pprV (BitV False) = "0"
pprV (BitsV b) = pprV (ArrayV (map BitV b))
pprV (IntV i) = show i
pprV (FunV {}) = error "pprV: no way to print function value"
pprV VoidV = error "pprV: no way to print void value"
pprV (PointerV Null) = "null"
pprV (PointerV {}) = error "pprV: no way to print non-null pointer value"

pprOp :: BinOp -> String
pprOp (ChoiceOp xs) = 
  let f [x] = pprOp x
      f (x:xs) = pprOp x ++ " | " ++ f xs
  in "(" ++ f xs ++ ")"
pprOp AndOp = "&"
pprOp LAndOp = "&&"
pprOp AddOp = "+"
pprOp SubOp = "-"
pprOp LtOp = "<"
pprOp GtOp = ">"
pprOp LeOp = "<="
pprOp GeOp = ">="
pprOp EqOp = "=="
pprOp NeqOp = "!="
pprOp OrOp = "|"
pprOp LOrOp = "||"
pprOp XorOp = "^"
pprOp MulOp = "*"
pprOp ModOp = "%"
pprOp DivOp = "/"
pprOp ShrOp = ">>"
pprOp ShlOp = "<<"

type Prec = Int
p_min = 0
p_max = length pbin

-- Precedence table for left associative binary operators:
-- Operators in the earlier groups have lower precedence
pbin :: [[String]]
pbin = [["||", "?"], ["&&"], ["|", "{|}"], ["^"], ["&"], ["==", "!="],
        ["<", ">", "<=", ">="], ["<<", ">>"],
        ["+", "-"], ["*", "%", "/"]]

pbinof :: [[String]] -> String -> Int
pbinof [] op = p_min
pbinof (x:xs) op
  | op `elem` x = 0
  | otherwise = 1 + pbinof xs op

-- | parens p p_op x
-- Parenthesize the given expression if p_op is lower than p.
parens :: Prec -> Prec -> String -> String
parens p p_op x
  | p_op < p = "(" ++ x ++ ")"
  | otherwise = x

-- | left p op a b
-- Pretty print a left associative binary operator in the given precedence.
left :: Prec -> String -> Expr -> Expr -> String
left p op a b =
  let p_op = pbinof pbin op
  in parens p p_op $ pprE p_op a ++ " " ++ op ++ " " ++ pprE (p_op+1) b

-- | Pretty print an expression.
-- If the precedence of the operator is lower than given p, then the
-- expression must be parenthesized.
pprE :: Prec -> Expr -> String
pprE p e =
  case e of
    ValE v -> pprV v
    BinaryE op a b -> left p (pprOp op) a b
    NotE a -> "!" ++ pprE p_max a
    CondE p a b -> pprE p_max p ++ " ? " ++ pprE p_max a ++ " : " ++ pprE p_max b
    PostIncrE a -> pprLV a ++ "++"
    PostDecrE a -> pprLV a ++ "--"
    PreIncrE a -> "++" ++ pprLV a
    PreDecrE a -> "--" ++ pprLV a
    PlusEqE a b -> pprLV a ++ " += " ++ pretty b
    ArrayE xs -> "{" ++ commas (map pretty xs) ++ "}"
    HoleE _ Nothing -> "??"
    HoleE _ (Just n) -> "??(" ++ show n ++ ")"
    ChoiceE a b -> "{|" ++ pretty a ++ " | " ++ pretty b ++ "|}"
    BitChooseE _ a b -> left p "{|}" a b
    VarE nm -> nm
    AccessE a b -> pretty a ++ "[" ++ pretty b ++ "]"
    BulkAccessE a b c -> pretty a ++ "[" ++ pretty b ++ "::" ++ pretty c ++ "]"
    FieldE x nm -> pretty x ++ "." ++ nm
    NewE nm fields ->
        let pfields [] = ""
            pfields [(n, v)] = n ++ " = " ++ pretty v
            pfields ((n,v):xs) = n ++ " = " ++ pretty v ++ "," ++ pfields xs
        in "new " ++ nm ++ "(" ++ pfields fields ++ ")"
    CastE t e -> "(" ++ pretty t ++ ") " ++ pprE p_max e
    ICastE _ e -> pprE p e
    AppE nm xs -> nm ++ "(" ++ commas (map pretty xs) ++ ")"


pprLV :: LVal -> String
pprLV (VarLV nm) = nm
pprLV (ArrLV lv idx) = pprLV lv ++ "[" ++ pretty idx ++ "]"
pprLV (BulkLV lv lo w) = pprLV lv ++ "[" ++ pretty lo ++ "::" ++ pretty w ++ "]"
pprLV (FieldLV lv m) = pprLV lv ++ "." ++ m
pprLV (ChoiceLV a b) = "{|" ++ pprLV a ++ " | " ++ pprLV b ++ "|}"

-- | Prefix the string to the first line of the given block.
infirst :: String -> [String] -> [String]
infirst x [] = [x]
infirst x (l:ls) = (x ++ l) : ls
    
-- | Pretty print a statement.
-- A statement is printed as a list of lines to facilitate indenting.
pprS :: Stmt -> [String]
pprS (ReturnS (ValE VoidV)) = ["return;"]
pprS (ReturnS x) = ["return " ++ pretty x ++ ";"]
pprS (ExprS x) = [pretty x ++ ";"]
pprS (ReorderS xs) = concat [["reorder {"], map ("    " ++) (concatMap pprS xs), ["}"]]
pprS (AssertS x) = ["assert " ++ pretty x ++ ";"]
pprS (RepeatS n s) = infirst ("repeat (" ++ show n ++ ") ") (pprS s)
pprS (ForS i cond incr b@(BlockS {}))
  = infirst ("for (" ++ concat (pprS i) ++ " " ++ pretty cond ++ "; " ++ init (concat (pprS incr)) ++ ") ") (pprS b)
pprS (ForS i cond incr b) = pprS (ForS i cond incr (BlockS [b]))
pprS (WhileS c s) = infirst ("while (" ++ pretty c ++ ") ") (pprS s)
pprS (DeclS ty vars) =
  let pvars [(n, Nothing)] = n
      pvars [(n, Just v)] = n ++ " = " ++ pretty v
      pvars ((n, Nothing):xs) = n ++ ", " ++ pvars xs
      pvars ((n, Just v):xs) = n ++ " = " ++ pretty v ++ ", " ++ pvars xs
  in [pretty ty ++ " " ++ pvars vars ++ ";"]
pprS (UpdateS lv ex) = [pprLV lv ++ " = " ++ pretty ex ++ ";"]
pprS (BlockS xs) = concat [["{"], map ("   " ++) (concatMap pprS xs), ["}"]]
pprS (IfS p a (BlockS []))
 = infirst ("if (" ++ pretty p ++ ") ") (pprS a)
pprS (IfS p a b)
 = infirst ("if (" ++ pretty p ++ ") ") $
     case (pprS a) of
        ls -> init ls ++ (infirst (last ls ++ " else ") (pprS b))

class Pretty a where
   pretty :: a -> String

instance Pretty Type where
   pretty = pprT

instance Pretty Value where
   pretty = pprV

instance Pretty Expr where
   pretty = pprE 0

instance Pretty BinOp where
   pretty = pprOp


instance Pretty Stmt where
   pretty s = unlines (pprS s)

instance Pretty Decl where
   pretty (FunD nm (Function oty xs body) kind) = 
     pprkindl kind ++ pretty oty ++ " " ++ nm ++ "(" ++ pretty xs ++ ")"
        ++ pprkindr kind ++ pretty body
   pretty (VarD ty nm x) = pretty ty ++ " " ++ nm ++ " = " ++ pretty x ++ ";"
   pretty (StructD nm fields) =
     let f (n, ty) = pretty ty ++ " " ++ n ++ ";"
     in "struct " ++ nm ++ " {\n" ++ unlines (map (("  " ++) . f) fields) ++ "}"

instance Pretty Arg where
   pretty (Arg nm ty False) = pretty ty ++ " " ++ nm
   pretty (Arg nm ty True) = "ref " ++ pretty ty ++ " " ++ nm

instance Pretty [Arg] where
   pretty [] = ""
   pretty [x] = pretty x
   pretty (x:xs) = pretty x ++ ", " ++ pretty xs

instance Pretty Program where
   pretty p = unlines (map pretty (decls p))
   
pprkindl :: FunctionKind -> String
pprkindl NormalF = ""
pprkindl GeneratorF = "generator "
pprkindl HarnessF = "harness "
pprkindl (WithSpecF {}) = ""

pprkindr :: FunctionKind -> String
pprkindr NormalF = " "
pprkindr GeneratorF = " "
pprkindr HarnessF = " "
pprkindr (WithSpecF v) = " implements " ++ v ++ " "

