
-- | Pretty printer for Sketch programs
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Ppr (pretty) where


import Smten.Prelude
import Bits
import Sketch

class Ppr a where
   pretty :: a -> String
   prettya :: a -> String
   prettya x = "(" ++ pretty x ++ ")"

instance Ppr Name where
   pretty = id
   prettya = pretty

instance Ppr Type where
   pretty BitT = "bit"
   pretty (BitsT n) = "bit[" ++ pretty n ++ "]"
   pretty IntT = "int"

   prettya = pretty

instance Ppr Int where
   pretty = show
   prettya = show

arrayargs :: [Expr] -> String
arrayargs [x] = pretty x
arrayargs (x:xs) = pretty x ++ ", " ++ arrayargs xs

instance Ppr Expr where
   pretty (AndE a b) = prettya a ++ " & " ++ prettya b
   pretty (AddE a b) = prettya a ++ " + " ++ prettya b
   pretty (SubE a b) = prettya a ++ " - " ++ prettya b
   pretty (LtE a b) = prettya a ++ " < " ++ prettya b
   pretty (GtE a b) = prettya a ++ " > " ++ prettya b
   pretty (ArrayE xs) = "{" ++ arrayargs xs ++ "}"
   pretty (OrE a b) = prettya a ++ " | " ++ prettya b
   pretty (XorE a b) = prettya a ++ " ^ " ++ prettya b
   pretty (MulE a b) = prettya a ++ " * " ++ prettya b
   pretty (NotE a) = "!" ++ prettya a
   pretty (ShlE a b) = prettya a ++ " << " ++ prettya b
   pretty (ShrE a b) = prettya a ++ " >> " ++ prettya b
   pretty (HoleE) = "??"
   pretty (BitE b) = if b then "true" else "false"
   pretty (BitsE n) = show (valB n)
   pretty (IntE n) = pretty n
   pretty (VarE nm) = pretty nm
   pretty (AccessE a b) = prettya a ++ "[" ++ pretty b ++ "]"
   pretty (CastE t e) = "(" ++ pretty t ++ ") " ++ prettya e

   prettya HoleE = "??"
   prettya (IntE n) = pretty n
   prettya (VarE nm) = pretty nm
   prettya x = "(" ++ pretty x ++ ")"

instance Ppr Stmt where
   pretty (ReturnS x) = "return " ++ prettya x ++ ";"
   pretty (AssertS x) = "assert " ++ prettya x ++ ";"
   pretty (DeclS ty nm) = pretty ty ++ " " ++ pretty nm ++ ";"
   pretty (UpdateS nm ex) = pretty nm ++ " = " ++ pretty ex ++ ";"
   pretty (ArrUpdateS nm i ex) = pretty nm ++ "[" ++ pretty i ++ "] = " ++ pretty ex ++ ";"
   pretty (BlockS xs) = "{\n" ++ (unlines (map (("   " ++) . pretty) xs)) ++ "\n}"
   pretty (IfS p a b) = "if (" ++ pretty p ++ ") " ++ pretty a ++ " else " ++ pretty b

instance Ppr Decl where
   pretty (FunD nm oty xs stmts spec) = 
     pretty oty ++ " " ++ pretty nm ++ "(" ++ pretty xs ++ ")"
        ++ pprspec spec ++ "{\n"
         ++ (unlines (map (("   " ++) . pretty) stmts))
        ++ "\n}"
   pretty (VarD ty nm x) = pretty ty ++ " " ++ pretty nm ++ " = " ++ pretty x ++ ";"

instance Ppr [(Type, Name)] where
   pretty [] = ""
   pretty [(t, x)] = pretty t ++ " " ++ pretty x
   pretty ((t, x):tl) = pretty t ++ " " ++ pretty x ++ ", " ++ pretty tl

instance Ppr Prog where
   pretty xs = unlines (map pretty xs)

pprspec :: Maybe Name -> String
pprspec Nothing = " "
pprspec (Just v) = " implements " ++ pretty v ++ " "

