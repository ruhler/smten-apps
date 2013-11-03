
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}
module Sketch (
    Prog, ProgEnv, Decl(..), Type(..), Name, Stmt(..),
    Expr(..), Value(..), Function(..), FunctionKind(..),
    FunctionInput, ProgramInput,
    envof, declsof, d_type,
    bnd_ctrlbits, bnd_unroll_amnt,
    blockS, typeofV, arrayV,
    ) where

import Smten.Prelude
import Smten.Derive.Show
import qualified Smten.Data.Map as Map

import Bits

type Name = String

data Type = 
    BitT              -- bit
  | ArrT Type Expr    -- T[n]
  | IntT              -- int
  | FunT Type [Type]  -- function type: output and argument types
  | UnknownT          -- type is not known

showsPrecType :: Int -> Type -> ShowS
showsPrecType = $(derive_showsPrec ''Type)

instance Show Type where
    showsPrec = showsPrecType

data Value = 
    ArrayV [Value]
  | BitV Bit
  | BitsV Bits
  | IntV Int
  | FunV Function

-- Make an array value.
-- Automatically constructs a BitsV if the argument type is Bit.
arrayV :: [Value] -> Value
arrayV xs =
  case xs of
    (BitV {} : _) -> BitsV (mkbits [v | BitV v <- xs])
    _ -> ArrayV xs

typeofV :: Value -> Type
typeofV (ArrayV []) = UnknownT
typeofV (ArrayV xs) = ArrT (typeofV (head xs)) (ValE (IntV (length xs)))
typeofV (BitV {}) = BitT
typeofV (BitsV b) = ArrT BitT (ValE (IntV $ width b))
typeofV (IntV v) = UnknownT -- could be any integer literal
typeofV (FunV f) = UnknownT



showsPrecValue :: Int -> Value -> ShowS
showsPrecValue = $(derive_showsPrec ''Value) 

instance Show Value where
    showsPrec = showsPrecValue

instance Eq Value where
    (==) (BitV a) (BitV b) = a == b
    (==) (BitsV a) (BitsV b) = a == b
    (==) (IntV a) (IntV b) = a == b
    (==) a b = error $ "Value.==: bad args: " ++ show (a, b)

data Expr = 
   ValE Value
 | AndE Expr Expr        -- ^ a & b
 | AddE Expr Expr        -- ^ a + b
 | SubE Expr Expr        -- ^ a - b
 | LtE Expr Expr         -- ^ a < b
 | GtE Expr Expr         -- ^ a > b
 | LeE Expr Expr         -- ^ a <= b
 | GeE Expr Expr         -- ^ a >= b
 | EqE Expr Expr         -- ^ a == b
 | NeqE Expr Expr        -- ^ a != b
 | OrE Expr Expr         -- ^ a | b
 | LOrE Expr Expr        -- ^ a || b
 | LAndE Expr Expr       -- ^ a && b
 | XorE Expr Expr        -- ^ a ^ b
 | MulE Expr Expr        -- ^ a * b
 | ModE Expr Expr        -- ^ a % b
 | NotE Expr             -- ^ ! a
 | ShrE Expr Expr        -- ^ a >> b
 | ShlE Expr Expr        -- ^ a << b
 | ArrayE [Expr]         -- ^ {a, b, ... }
 | HoleE Int             -- ^ ??(n)      n is the number of bits to use
 | BitChooseE Expr Expr  -- ^ a {|} b
 | VarE Name             -- ^ foo
 | AccessE Expr Expr     -- ^ foo[i]    Note: i has type Int
 | CastE Type Expr       -- ^ (T) e
 | AppE Name [Expr]      -- ^ f(x, y, ...)

showsPrecExpr :: Int -> Expr -> ShowS
showsPrecExpr = $(derive_showsPrec ''Expr)

instance Show Expr where
    showsPrec = showsPrecExpr

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

-- Construct a block of statements.
-- This flattens any blocks into the higher level.
blockS :: [Stmt] -> Stmt
blockS xs =
  let f :: Stmt -> [Stmt]
      f (BlockS xs) = concatMap f xs
      f s = [s]
  in BlockS (concatMap f xs)

showsPrecStmt :: Int -> Stmt -> ShowS
showsPrecStmt = $(derive_showsPrec ''Stmt)
   
instance Show Stmt where
    showsPrec = showsPrecStmt

data Function = Function {
    f_type :: Type,
    f_args :: [Name],
    f_body :: Stmt
}

showsPrecFunction :: Int -> Function -> ShowS
showsPrecFunction = $(derive_showsPrec ''Function)

instance Show Function where
  showsPrec = showsPrecFunction

data FunctionKind = NormalF          -- ^ a normal function
                  | WithSpecF Name   -- ^ the function has a spec
                  | GeneratorF       -- ^ the function is a generator

showsPrecFunctionKind :: Int -> FunctionKind -> ShowS
showsPrecFunctionKind = $(derive_showsPrec ''FunctionKind)

instance Show FunctionKind where
    showsPrec = showsPrecFunctionKind

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

d_type :: Decl -> Type
d_type d@(FunD {}) = f_type $ fd_val d
d_type d@(VarD {}) = vd_ty d

showsPrecDecl :: Int -> Decl -> ShowS
showsPrecDecl = $(derive_showsPrec ''Decl)

instance Show Decl where
    showsPrec = showsPrecDecl

type Prog = [Decl]
type ProgEnv = Map.Map String Decl

envof :: Prog -> ProgEnv
envof p = Map.fromList [(d_name d, d) | d <- p]

declsof :: ProgEnv -> Prog
declsof p = Map.elems p

-- The input to a function is the list of its arguments.
type FunctionInput = [Value]

-- The input to a program is a sample function input for each of its top level
-- harnesses.
type ProgramInput = Map.Map String FunctionInput

-- TODO: don't hardcode --bnd-ctrlbits like this.
bnd_ctrlbits :: Int
bnd_ctrlbits = 5

bnd_unroll_amnt :: Int
bnd_unroll_amnt = 8

