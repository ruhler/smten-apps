
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}
module Sketch (
    Prog, Decl(..), Type(..), Name, Stmt(..),
    Expr(..),
    ) where

import Smten.Prelude
import qualified Smten.Data.Map as Map

type Name = String

data Type = 
    BitT              -- bit
  | BitVectorT Int    -- bit[n]

instance Show Type where
    show BitT = "BitT"
    show (BitVectorT n) = "BitVectorT " ++ show n

data Expr = 
   AndE Expr Expr        -- ^ a & b
 | OrE Expr Expr         -- ^ a | b
 | HoleE                       -- ^ ??
 | IntE Int                -- ^ 42
 | VarE Name                   -- ^ foo
 | AccessE Expr Expr       -- ^ foo[i]

instance Show Expr where
    show (AndE a b) = "AndE " ++ show a ++ " " ++ show b
    show (OrE a b) = "OrE " ++ show a ++ " " ++ show b
    show HoleE = "HoleE"
    show (IntE x) = "IntE " ++ show x
    show (VarE n) = "VarE " ++ show n
    show (AccessE a b) = "AccessE " ++ show a ++ " " ++ show b

data Stmt = ReturnS Expr

instance Show Stmt where
    show (ReturnS x) = "ReturnS " ++ show x

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

