
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
   pretty (EqE a b) = prettya a ++ " == " ++ prettya b
   pretty (ArrayE xs) = "{" ++ arrayargs xs ++ "}"
   pretty (OrE a b) = prettya a ++ " | " ++ prettya b
   pretty (XorE a b) = prettya a ++ " ^ " ++ prettya b
   pretty (MulE a b) = prettya a ++ " * " ++ prettya b
   pretty (NotE a) = "!" ++ prettya a
   pretty (ShlE a b) = prettya a ++ " << " ++ prettya b
   pretty (ShrE a b) = prettya a ++ " >> " ++ prettya b
   pretty (HoleE v) = "??(" ++ show v ++ ")"
   pretty (BitE b) = if b then "true" else "false"
   pretty (BitsE n) = show (valB n)
   pretty (IntE n) = pretty n
   pretty (VarE nm) = pretty nm
   pretty (AccessE a b) = prettya a ++ "[" ++ pretty b ++ "]"
   pretty (CastE t e) = "(" ++ pretty t ++ ") " ++ prettya e
   pretty (AppE f xs) =
     let pargs [] = ""
         pargs [x] = pretty x
         pargs (x:xs) = pretty x ++ ", " ++ pargs xs
     in pretty f ++ "(" ++ pargs xs ++ ")"
   pretty (FunE f) = error $ "No way to pretty print an anonymous function"

   prettya (HoleE v) = "??(" ++ show v ++ ")"
   prettya (IntE n) = pretty n
   prettya (VarE nm) = pretty nm
   prettya x = "(" ++ pretty x ++ ")"

instance Ppr Stmt where
   pretty (ReturnS x) = "return " ++ prettya x ++ ";"
   pretty (AssertS x) = "assert " ++ prettya x ++ ";"
   pretty (RepeatS n s) = "repeat (" ++ pretty n ++ ") " ++ pretty s
   pretty (DeclS ty nm) = pretty ty ++ " " ++ pretty nm ++ ";"
   pretty (UpdateS nm ex) = pretty nm ++ " = " ++ pretty ex ++ ";"
   pretty (ArrUpdateS nm i ex) = pretty nm ++ "[" ++ pretty i ++ "] = " ++ pretty ex ++ ";"
   pretty (BlockS xs) = "{\n" ++ (unlines (map (("   " ++) . pretty) xs)) ++ "\n}"
   pretty (IfS p a b) = "if (" ++ pretty p ++ ") " ++ pretty a ++ " else " ++ pretty b

instance Ppr Decl where
   pretty (FunD nm (Function oty xs body) kind) = 
     pprkindl kind ++ pretty oty ++ " " ++ pretty nm ++ "(" ++ pretty xs ++ ")"
        ++ pprkindr kind ++ pretty body
   pretty (VarD ty nm x) = pretty ty ++ " " ++ pretty nm ++ " = " ++ pretty x ++ ";"

instance Ppr [(Type, Name)] where
   pretty [] = ""
   pretty [(t, x)] = pretty t ++ " " ++ pretty x
   pretty ((t, x):tl) = pretty t ++ " " ++ pretty x ++ ", " ++ pretty tl

instance Ppr Prog where
   pretty xs = unlines (map pretty xs)

pprkindl :: FunctionKind -> String
pprkindl NormalF = ""
pprkindl GeneratorF = "generator "
pprkindl (WithSpecF {}) = ""

pprkindr :: FunctionKind -> String
pprkindr NormalF = " "
pprkindr GeneratorF = " "
pprkindr (WithSpecF v) = " implements " ++ pretty v ++ " "

