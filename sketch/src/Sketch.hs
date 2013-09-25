
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}
module Sketch (
    Prog, Decl(..), Type(..), Name, Stmt(..),
    Expr(..),
    valEq,
    ) where

import Smten.Prelude
import qualified Smten.Data.Map as Map

import Bits

type Name = String

data Type = 
    BitT              -- bit
  | BitsT Int         -- bit[n]
  | IntT              -- int
  | UnknownT          -- type to be inferred during type inference.

instance Show Type where
    show BitT = "BitT"
    show (BitsT n) = "BitsT " ++ show n
    show IntT = "IntT"

data Expr = 
   AndE Expr Expr        -- ^ a & b
 | OrE Expr Expr         -- ^ a | b
 | XorE Expr Expr        -- ^ a ^ b
 | MulE Expr Expr        -- ^ a * b
 | NotE Expr             -- ^ ! a
 | ShrE Expr Expr        -- ^ a >> b
 | ShlE Expr Expr        -- ^ a << b
 | HoleE Type            -- ^ ??
 | BitE Bit              -- ^ 1
 | BitsE Bits            -- ^ 4'h2
 | IntE Int              -- ^ 42
 | VarE Name             -- ^ foo
 | AccessE Expr Expr     -- ^ foo[i]    Note: i has type Int
 | ErrE String           -- ^ used for errors

instance Show Expr where
    show (AndE a b) = "AndE " ++ show a ++ " " ++ show b
    show (OrE a b) = "OrE " ++ show a ++ " " ++ show b
    show (MulE a b) = "MulE " ++ show a ++ " " ++ show b
    show (XorE a b) = "XorE " ++ show a ++ " " ++ show b
    show (ShrE a b) = "ShrE " ++ show a ++ " " ++ show b
    show (ShlE a b) = "ShlE " ++ show a ++ " " ++ show b
    show (NotE a) = "NotE " ++ show a
    show (HoleE _) = "HoleE"
    show (BitE b) = "BitE " ++ show b
    show (BitsE b) = "BitsE " ++ show b
    show (IntE x) = "IntE " ++ show x
    show (VarE n) = "VarE " ++ show n
    show (AccessE a b) = "AccessE " ++ show a ++ " " ++ show b

data Stmt =
     ReturnS Expr                   -- ^ return e;
   | DeclS Type Name                -- ^ ty foo;
   | UpdateS Name Expr              -- ^ foo = e;
   | ArrUpdateS Name Expr Expr      -- ^ foo[e1] = e2;
   | IfS Expr Stmt Stmt             -- ^ if (e) s1 else s2
   | BlockS [Stmt]                  -- ^ { stmts }
   

instance Show Stmt where
    show (ReturnS x) = "ReturnS " ++ show x
    show (DeclS ty nm) = "DeclS " ++ show ty ++ " " ++ show nm
    show (UpdateS nm ex) = "UpdateS " ++ show nm ++ " " ++ show ex
    show (ArrUpdateS nm i ex) = "ArrUpdateS " ++ show nm ++ " " ++ show i ++ " " ++ show ex
    show (IfS p a b) = "IfS " ++ show p ++ " " ++ show a ++ " " ++ show b
    show (BlockS xs) = "BlockS " ++ show xs

data Decl = FunD {
  fd_name :: Name,
  fd_outty :: Type,
  fd_args :: [(Type, Name)],
  fd_stmts :: [Stmt],
  
  -- | Nothing means this declaration is a specification
  --   Just foo means this is a sketch with specification 'foo'
  fd_spec :: Maybe Name
}

instance Show Decl where
    show x = "FunD { " ++
      "nm = " ++ show (fd_name x) ++ ", " ++
      "oty = " ++ show (fd_outty x) ++ ", " ++
      "args = " ++ show (fd_args x) ++ ", " ++
      "stmts = " ++ show (fd_stmts x) ++ ", " ++
      "spec = " ++ show (fd_spec x) ++ "}"

type Prog = [Decl]

valEq :: Expr -> Expr -> Bool
valEq (BitE a) (BitE b) = a == b
valEq (BitsE a) (BitsE b) = a == b
valEq (IntE a) (IntE b) = a == b
valEq _ _ = error "valEq: bad args"


