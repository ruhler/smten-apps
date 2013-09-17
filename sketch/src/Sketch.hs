
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}
module Sketch (
    SKProg, SKDecl(..), SKType(..), Name, SKStmt(..),
    SKExpr(..),
    ) where

import Smten.Prelude
import qualified Smten.Data.Map as Map

type HoleID = Integer
type Name = String

data SKType = 
    SKBit                       -- bit
  | SKBitVector Int         -- bit[n]

instance Show SKType where
    show SKBit = "SKBit"
    show (SKBitVector n) = "SKBitVector " ++ show n

data SKExpr = 
   SKBVAnd SKExpr SKExpr        -- ^ a & b
 | SKBVOr SKExpr SKExpr         -- ^ a | b
 | SKHole HoleID                -- ^ ??
 | SKInt Int                -- ^ 42
 | SKVar Name                   -- ^ foo
 | SKAccess SKExpr SKExpr       -- ^ foo[i]

instance Show SKExpr where
    show (SKBVAnd a b) = "SKBVAnd " ++ show a ++ " " ++ show b
    show (SKBVOr a b) = "SKBVOr " ++ show a ++ " " ++ show b
    show (SKHole x) = "SKHole " ++ show x
    show (SKInt x) = "SKInt " ++ show x
    show (SKVar n) = "SKVar " ++ show n
    show (SKAccess a b) = "SKAccess " ++ show a ++ " " ++ show b

data SKStmt = SKReturn SKExpr

instance Show SKStmt where
    show (SKReturn x) = "SKReturn " ++ show x

data SKDecl =
   SKFun {
    skf_name :: Name,
    skf_outty :: SKType,
    skf_args :: [(SKType, Name)],
    skf_stmts :: [SKStmt],
    skf_spec :: Maybe Name }

instance Show SKDecl where
    show x = "SKFun { " ++
      "nm = " ++ show (skf_name x) ++ ", " ++
      "oty = " ++ show (skf_outty x) ++ ", " ++
      "args = " ++ show (skf_args x) ++ ", " ++
      "stmts = " ++ show (skf_stmts x) ++ ", " ++
      "spec = " ++ show (skf_spec x) ++ "}"

type SKProg = [SKDecl]

