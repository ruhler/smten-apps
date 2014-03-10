
-- | Pretty printer for Sketch programs
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Ppr (pretty) where


import Smten.Prelude
import Sketch

class Ppr a where
   pretty :: a -> String
   prettya :: a -> String
   prettya x = "(" ++ pretty x ++ ")"

instance Ppr Name where
   pretty = id
   prettya = pretty

instance Ppr Type where
   pretty VoidT = "void"
   pretty BitT = "bit"
   pretty (ArrT t n) = pretty t ++ "[" ++ pretty n ++ "]"
   pretty IntT = "int"

   prettya = pretty

instance Ppr Int where
   pretty = show
   prettya = show

commas :: (Ppr a) => [a] -> String
commas [x] = pretty x
commas (x:xs) = pretty x ++ ", " ++ commas xs

instance Ppr Value where
   pretty (ArrayV xs) = "{" ++ commas xs ++ "}"
   pretty (BitV b) = if b then "true" else "false"
   pretty (BitsV n) = pretty (ArrayV (map BitV n))
   pretty (IntV n) = pretty n
   pretty (FunV f) = error $ "No way to pretty print an anonymous function"
   pretty VoidV = error $ "No way to pretty print a void value"

instance Ppr Expr where
   pretty (ValE v) = pretty v
   pretty (AndE a b) = prettya a ++ " & " ++ prettya b
   pretty (LAndE a b) = prettya a ++ " && " ++ prettya b
   pretty (AddE a b) = prettya a ++ " + " ++ prettya b
   pretty (SubE a b) = prettya a ++ " - " ++ prettya b
   pretty (ModE a b) = prettya a ++ " % " ++ prettya b
   pretty (DivE a b) = prettya a ++ " / " ++ prettya b
   pretty (LtE a b) = prettya a ++ " < " ++ prettya b
   pretty (GtE a b) = prettya a ++ " > " ++ prettya b
   pretty (LeE a b) = prettya a ++ " <= " ++ prettya b
   pretty (GeE a b) = prettya a ++ " >= " ++ prettya b
   pretty (EqE a b) = prettya a ++ " == " ++ prettya b
   pretty (NeqE a b) = prettya a ++ " != " ++ prettya b
   pretty (ArrayE xs) = "{" ++ commas xs ++ "}"
   pretty (OrE a b) = prettya a ++ " | " ++ prettya b
   pretty (LOrE a b) = prettya a ++ " || " ++ prettya b
   pretty (XorE a b) = prettya a ++ " ^ " ++ prettya b
   pretty (MulE a b) = prettya a ++ " * " ++ prettya b
   pretty (NotE a) = "!" ++ prettya a
   pretty (CondE p a b) = prettya p ++ " ? " ++ prettya a ++ " : " ++ prettya b
   pretty (ShlE a b) = prettya a ++ " << " ++ prettya b
   pretty (ShrE a b) = prettya a ++ " >> " ++ prettya b
   pretty (HoleE _ (Just v)) = "??(" ++ show v ++ ")"
   pretty (HoleE _ Nothing) = "??"
   pretty (BitChooseE _ a b) = prettya a ++ " {|} " ++ prettya b
   pretty (VarE nm) = pretty nm
   pretty (AccessE a b) = prettya a ++ "[" ++ pretty b ++ "]"
   pretty (BulkAccessE a b c)
    = prettya a ++ "[" ++ pretty b ++ "::" ++ pretty c ++ "]"
   pretty (CastE t e) = "(" ++ pretty t ++ ") " ++ prettya e
   pretty (ICastE _ e) = pretty e
   pretty (AppE f xs) =
     let pargs [] = ""
         pargs [x] = pretty x
         pargs (x:xs) = pretty x ++ ", " ++ pargs xs
     in pretty f ++ "(" ++ pargs xs ++ ")"

   prettya (HoleE _ (Just v)) = "??(" ++ show v ++ ")"
   prettya (HoleE _ Nothing) = "??"
   prettya (VarE nm) = pretty nm
   prettya x = "(" ++ pretty x ++ ")"

infirst :: String -> [String] -> [String]
infirst x [] = [x]
infirst x (l:ls) = (x ++ l) : ls

-- A statement is printed as a list of lines
pprS :: Stmt -> [String]
pprS (ReturnS (ValE VoidV)) = ["return;"]
pprS (ReturnS x) = ["return " ++ prettya x ++ ";"]
pprS (ExprS x) = [prettya x ++ ";"]
pprS (ReorderS xs) = concat [["reorder {"], map ("    " ++) (concatMap pprS xs), ["}"]]
pprS (AssertS x) = ["assert " ++ prettya x ++ ";"]
pprS (RepeatS n s) = infirst ("repeat (" ++ pretty n ++ ") ") (pprS s)
pprS (ForS init cond incr b)
 = infirst ("for (" ++ pretty init ++ pretty cond ++ ";" ++ pretty incr ++ ") ") (pprS b)
pprS (WhileS c s) = infirst ("while (" ++ pretty c ++ ") ") (pprS s)
pprS (DeclS ty nm) = [pretty ty ++ " " ++ pretty nm ++ ";"]
pprS (UpdateS lv ex) = [pretty lv ++ " = " ++ pretty ex ++ ";"]
pprS (BlockS xs) = concat [["{"], map ("   " ++) (concatMap pprS xs), ["}"]]
pprS (IfS p a (BlockS []))
 = infirst ("if (" ++ pretty p ++ ") ") (pprS a)
pprS (IfS p a b)
 = infirst ("if (" ++ pretty p ++ ") ") $
     case (pprS a) of
        ls -> init ls ++ (infirst (last ls ++ " else ") (pprS b))

instance Ppr Stmt where
   pretty s = unlines (pprS s)

instance Ppr LVal where
   pretty (VarLV nm) = pretty nm
   pretty (ArrLV lv idx) = pretty lv ++ "[" ++ pretty idx ++ "]"
   pretty (BulkLV lv lo w) = pretty lv ++ "[" ++ pretty lv ++ "::" ++ pretty w ++ "]"

instance Ppr Arg where
   pretty (Arg nm ty False) = pretty ty ++ " " ++ pretty nm
   pretty (Arg nm ty True) = pretty "ref " ++ pretty ty ++ " " ++ pretty nm

instance Ppr Decl where
   pretty (FunD nm (Function oty xs body) kind) = 
     pprkindl kind ++ pretty oty ++ " " ++ pretty nm ++ "(" ++ pretty xs ++ ")"
        ++ pprkindr kind ++ pretty body
   pretty (VarD ty nm x) = pretty ty ++ " " ++ pretty nm ++ " = " ++ pretty x ++ ";"

instance Ppr [Arg] where
   pretty [] = ""
   pretty [x] = pretty x
   pretty (x:xs) = pretty x ++ ", " ++ pretty xs

instance Ppr Prog where
   pretty xs = unlines (map pretty xs)

pprkindl :: FunctionKind -> String
pprkindl NormalF = ""
pprkindl GeneratorF = "generator "
pprkindl HarnessF = "harness "
pprkindl (WithSpecF {}) = ""

pprkindr :: FunctionKind -> String
pprkindr NormalF = " "
pprkindr GeneratorF = " "
pprkindr HarnessF = " "
pprkindr (WithSpecF v) = " implements " ++ pretty v ++ " "

