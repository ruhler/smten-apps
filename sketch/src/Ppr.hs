
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

instance Ppr Expr where
   pretty (AndE a b) = prettya a ++ " & " ++ prettya b
   pretty (OrE a b) = prettya a ++ " | " ++ prettya b
   pretty (NotE a) = "!" ++ prettya a
   pretty (HoleE _) = "??"
   pretty (BitE b) = if b then "true" else "false"
   pretty (BitsE n) = show (valB n)
   pretty (IntE n) = pretty n
   pretty (VarE nm) = pretty nm
   pretty (AccessE a b) = prettya a ++ "[" ++ pretty b ++ "]"

   prettya (HoleE _) = "??"
   prettya (IntE n) = pretty n
   prettya (VarE nm) = pretty nm
   prettya x = "(" ++ pretty x ++ ")"

instance Ppr Stmt where
   pretty (ReturnS x) = "return " ++ prettya x ++ ";"
   pretty (DeclS ty nm ex) = pretty ty ++ " " ++ pretty nm ++ " = " ++ pretty ex ++ ";"
   pretty (UpdateS nm ex) = pretty nm ++ " = " ++ pretty ex ++ ";"
   pretty (ArrUpdateS nm i ex) = pretty nm ++ "[" ++ pretty i ++ "] = " ++ pretty ex ++ ";"

instance Ppr Decl where
   pretty (FunD nm oty xs stmts spec) = 
     pretty oty ++ " " ++ pretty nm ++ "(" ++ pretty xs ++ ")"
        ++ pprspec spec ++ "{\n"
         ++ (unlines (map (("   " ++) . pretty) stmts))
        ++ "\n}"

instance Ppr [(Type, Name)] where
   pretty [] = ""
   pretty [(t, x)] = pretty t ++ " " ++ pretty x
   pretty ((t, x):tl) = pretty t ++ " " ++ pretty x ++ ", " ++ pretty tl

instance Ppr Prog where
   pretty xs = unlines (map pretty xs)

pprspec :: Maybe Name -> String
pprspec Nothing = " "
pprspec (Just v) = " implements " ++ pretty v ++ " "

