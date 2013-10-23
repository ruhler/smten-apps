
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}
module Sketch (
    Prog, ProgEnv, Decl(..), Type(..), Name, Stmt(..),
    Expr(..), Function(..), FunctionKind(..),
    FunctionInput, ProgramInput,
    valEq, envof, declsof,
    bnd_ctrlbits,
    ) where

import Smten.Prelude
import qualified Smten.Data.Map as Map

import Bits

type Name = String

data Type = 
    BitT              -- bit
  | BitsT Expr        -- bit[n]
  | IntT              -- int
  | UnknownT          -- type is not known

instance Show Type where
    show BitT = "BitT"
    show (BitsT n) = "BitsT " ++ show n
    show IntT = "IntT"

data Expr = 
   AndE Expr Expr        -- ^ a & b
 | AddE Expr Expr        -- ^ a + b
 | SubE Expr Expr        -- ^ a - b
 | LtE Expr Expr         -- ^ a < b
 | GtE Expr Expr         -- ^ a > b
 | EqE Expr Expr         -- ^ a == b
 | OrE Expr Expr         -- ^ a | b
 | XorE Expr Expr        -- ^ a ^ b
 | MulE Expr Expr        -- ^ a * b
 | NotE Expr             -- ^ ! a
 | ShrE Expr Expr        -- ^ a >> b
 | ShlE Expr Expr        -- ^ a << b
 | ArrayE [Expr]         -- ^ {a, b, ... }
 | HoleE Int             -- ^ ??(n)      n is the number of bits to use
 | BitChooseE Expr Expr  -- ^ a {|} b
 | BitE Bit              -- ^ 1
 | BitsE Bits            -- ^ 4'h2
 | IntE Int              -- ^ 42
 | VarE Name             -- ^ foo
 | AccessE Expr Expr     -- ^ foo[i]    Note: i has type Int
 | ErrE String           -- ^ used for errors
 | CastE Type Expr       -- ^ (T) e
 | FunE Function         -- ^ anonymous function literal
 | AppE Expr [Expr]      -- ^ f(x, y, ...)

instance Show Expr where
    show (AndE a b) = "AndE " ++ show a ++ " " ++ show b
    show (AddE a b) = "AddE " ++ show a ++ " " ++ show b
    show (SubE a b) = "SubE " ++ show a ++ " " ++ show b
    show (GtE a b) = "GtE " ++ show a ++ " " ++ show b
    show (LtE a b) = "LtE " ++ show a ++ " " ++ show b
    show (ArrayE b) = "ArrayE " ++ show b
    show (OrE a b) = "OrE " ++ show a ++ " " ++ show b
    show (MulE a b) = "MulE " ++ show a ++ " " ++ show b
    show (XorE a b) = "XorE " ++ show a ++ " " ++ show b
    show (ShrE a b) = "ShrE " ++ show a ++ " " ++ show b
    show (ShlE a b) = "ShlE " ++ show a ++ " " ++ show b
    show (NotE a) = "NotE " ++ show a
    show (HoleE m) = "HoleE " ++ show m
    show (BitChooseE a b) = "BitChooseE " ++ show a ++ " " ++ show b
    show (BitE b) = "BitE " ++ show b
    show (BitsE b) = "BitsE " ++ show b
    show (IntE x) = "IntE " ++ show x
    show (VarE n) = "VarE " ++ show n
    show (AccessE a b) = "AccessE " ++ show a ++ " " ++ show b
    show (CastE t e) = "CastE " ++ show t ++ " " ++ show e
    show (FunE f) = "FunE " ++ show f
    show (AppE f xs) = "AppE " ++ show f ++ " " ++ show xs

data Stmt =
     ReturnS Expr                   -- ^ return e;
   | AssertS Expr                   -- ^ assert e;
   | RepeatS Expr Stmt              -- ^ repeat (n) s
   | WhileS Expr Stmt               -- ^ while (c) s
   | ForS Stmt Expr Stmt Stmt       -- ^ for (init ; cond ; incr ) body
   | DeclS Type Name                -- ^ ty foo;
   | UpdateS Name Expr              -- ^ foo = e;
   | ArrUpdateS Name Expr Expr      -- ^ foo[e1] = e2;
   | IfS Expr Stmt Stmt             -- ^ if (e) s1 else s2
   | BlockS [Stmt]                  -- ^ { stmts }
   

instance Show Stmt where
    show (ReturnS x) = "ReturnS " ++ show x
    show (AssertS x) = "AssertS " ++ show x
    show (RepeatS n s) = "RepeatS " ++ show n ++ " " ++ show s
    show (WhileS c s) = "WhileS " ++ show c ++ " " ++ show s
    show (ForS a b c d) = "ForS " ++ show a ++ " " ++ show b ++ " " ++ show c ++ " " ++ show d
    show (DeclS ty nm) = "DeclS " ++ show ty ++ " " ++ show nm
    show (UpdateS nm ex) = "UpdateS " ++ show nm ++ " " ++ show ex
    show (ArrUpdateS nm i ex) = "ArrUpdateS " ++ show nm ++ " " ++ show i ++ " " ++ show ex
    show (IfS p a b) = "IfS " ++ show p ++ " " ++ show a ++ " " ++ show b
    show (BlockS xs) = "BlockS " ++ show xs

data Function = Function {
    f_outty :: Type,
    f_args :: [(Type, Name)],
    f_body :: Stmt
}

instance Show Function where
  show x = "Function { " ++
    "oty = " ++ show (f_outty x) ++ ", " ++
    "args = " ++ show (f_args x) ++ ", " ++
    "body = " ++ show (f_body x) ++ "}"

data FunctionKind = NormalF          -- ^ a normal function
                  | WithSpecF Name   -- ^ the function has a spec
                  | GeneratorF       -- ^ the function is a generator

instance Show FunctionKind where
    show NormalF = "NormalF"
    show (WithSpecF nm) = "WithSpecF " ++ nm
    show GeneratorF = "GeneratorF"

data Decl =
   FunD {
      d_name :: Name,
      fd_val :: Function,
      fd_kind :: FunctionKind }
 | VarD {
      vd_ty :: Type,
      d_name :: Name,
      vd_val :: Expr
   }

instance Show Decl where
    show x@(FunD {}) = "FunD { " ++
      "nm = " ++ show (d_name x) ++ ", " ++
      "val = " ++ show (fd_val x) ++ ", " ++
      "kind = " ++ show (fd_kind x) ++ "}"
    show x@(VarD {}) = "VarD { " ++
      "ty = " ++ show (vd_ty x) ++ ", " ++
      "nm = " ++ show (d_name x) ++ ", " ++
      "val = " ++ show (vd_val x) ++ "}"

type Prog = [Decl]
type ProgEnv = Map.Map String Decl

envof :: Prog -> ProgEnv
envof p = Map.fromList [(d_name d, d) | d <- p]

declsof :: ProgEnv -> Prog
declsof p = Map.elems p

valEq :: Expr -> Expr -> Bool
valEq (BitE a) (BitE b) = a == b
valEq (BitsE a) (BitsE b) = a == b
valEq (IntE a) (IntE b) = a == b
valEq a b = error $ "valEq: bad args: " ++ show (a, b)


-- The input to a function is the list of its arguments.
type FunctionInput = [Expr]

-- The input to a program is a sample function input for each of its top level
-- harnesses.
type ProgramInput = Map.Map String FunctionInput

-- TODO: don't hardcode --bnd-ctrlbits like this.
bnd_ctrlbits :: Int
bnd_ctrlbits = 5

