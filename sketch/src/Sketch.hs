
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}
module Sketch (
    Prog, ProgEnv, Decl(..), Type(..), Name, Stmt(..),
    Expr(..), Value(..), Function(..), FunctionKind(..),
    FunctionInput, ProgramInput,
    Options(..), defaultOptions,
    envof, declsof, d_type, d_val,
    blockS, typeofV, dimension, arrayV, pad,
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

instance Eq Type where
    (==) BitT BitT = True
    (==) (ArrT a (ValE (IntV aw))) (ArrT b (ValE (IntV bw))) =
        a == b && aw == bw
    (==) (ArrT a wa) (ArrT b wb)
       | a == b = error $ "type equality on arrays with unknown width: " ++ show (wa, wb)
    (==) IntT IntT = True
    (==) (FunT a as) (FunT b bs) = a == b && as == bs
    (==) _ _ = False

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
    (BitV {} : _) -> BitsV [v | BitV v <- xs]
    _ -> ArrayV xs

typeofV :: Value -> Type
typeofV (ArrayV []) = ArrT UnknownT (ValE (IntV 0))
typeofV (ArrayV xs) = ArrT (typeofV (head xs)) (ValE (IntV (length xs)))
typeofV (BitV {}) = BitT
typeofV (BitsV b) = ArrT BitT (ValE (IntV $ length b))
typeofV (IntV 0) = BitT     -- it may be a bit literal, so indicate that:
typeofV (IntV 1) = BitT     --  it will promote to int if needed
typeofV (IntV w) = IntT
typeofV (FunV f) = UnknownT

dimension :: Type -> Int
dimension BitT = 1
dimension (ArrT t _) = 1 + dimension t
dimension IntT = 1
dimension (FunT {}) = 1


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
 | LAndE Expr Expr       -- ^ a && b
 | AddE Expr Expr        -- ^ a + b
 | SubE Expr Expr        -- ^ a - b
 | LtE Expr Expr         -- ^ a < b
 | GtE Expr Expr         -- ^ a > b
 | LeE Expr Expr         -- ^ a <= b
 | GeE Expr Expr         -- ^ a >= b
 | EqE Expr Expr         -- ^ a == b
 | NeqE Expr Expr        -- ^ a != b
 | CondE Expr Expr Expr  -- ^ p ? a : b
 | OrE Expr Expr         -- ^ a | b
 | LOrE Expr Expr        -- ^ a || b
 | XorE Expr Expr        -- ^ a ^ b
 | MulE Expr Expr        -- ^ a * b
 | ModE Expr Expr        -- ^ a % b
 | DivE Expr Expr        -- ^ a / b
 | NotE Expr             -- ^ ! a
 | ShrE Expr Expr        -- ^ a >> b
 | ShlE Expr Expr        -- ^ a << b
 | ArrayE [Expr]         -- ^ {a, b, ... }
 | HoleE Type (Maybe Int)   -- ^ ??(n)      n is the number of bits to use
 | BitChooseE Type Expr Expr  -- ^ a {|} b
 | VarE Name             -- ^ foo
 | AccessE Expr Expr     -- ^ foo[i]    Note: i has type Int
 | BulkAccessE Expr Expr Expr -- ^ foo[lo::N]
 | CastE Type Expr       -- ^ (T) e
 | ICastE Type Expr      -- ^ implicit cast of an expr to a given type
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
   | ArrBulkUpdateS Name Expr Expr Expr -- ^ foo[e1::e2] = e3;
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

d_val :: Decl -> Expr
d_val x =
    case x of
        VarD {} -> vd_val x
        FunD {} -> ValE (FunV (fd_val x))

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

data Options = Options {
  bnd_cbits :: Int,
  bnd_inbits :: Int,
  bnd_unroll_amnt :: Int,
  bnd_inline_amnt :: Int
}

showsPrecOptions :: Int -> Options -> ShowS
showsPrecOptions = $(derive_showsPrec ''Options)

instance Show Options where
    showsPrec = showsPrecOptions

defaultOptions :: Options
defaultOptions = Options {
    bnd_cbits = 5,
    bnd_inbits = 5,
    bnd_unroll_amnt = 8,
    bnd_inline_amnt = 5
}

pad :: Type -> Value
pad BitT = BitV False
pad IntT = IntV 0
pad (ArrT t (ValE (IntV w))) = arrayV (replicate w (pad t))

