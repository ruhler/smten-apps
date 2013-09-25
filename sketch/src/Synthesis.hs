
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}
module Synthesis (
    synthesize,
    ) where

import Smten.Prelude
import Smten.Control.Monad
import qualified Smten.Data.Map as Map
import Smten.Data.Functor
import Smten.Symbolic
import Smten.Symbolic.SMT

import Bits
import Cegis
import Eval
import Sketch

synthesize :: Prog -> SMT Prog
synthesize p = do
  let specs = Map.fromList [(fd_name x, x) | x <- p]
  mapM (synthesizeD specs) p

-- Synthesize a single declaration
synthesizeD :: Map.Map Name Decl -> Decl -> SMT Decl
synthesizeD specs d =
  case fd_spec d of
    Nothing -> return d
    Just s ->
     case Map.lookup s specs of
        Nothing -> error $ "Spec " ++ s ++ " not found for sketch " ++ show (fd_name d)
        Just sd -> do
          let p :: [Stmt] -> [Expr] -> Bool
              p stmts vars =
                 let specargs = Map.fromList (zip (map snd (fd_args sd)) vars)
                     want = evalP (fd_stmts sd) specargs
                      
                     sketchargs = Map.fromList (zip (map snd (fd_args d)) vars)
                     got = evalP stmts sketchargs
                 in want `valEq` got
          res <- cegis (mkFreeArgs (map fst (fd_args d))) (deHoleStmts (fd_stmts d)) [] p
          case res of
            Just stmts' -> return (d { fd_stmts = stmts' })
            Nothing -> error $ "Sketch " ++ show (fd_name d) ++ " unsatisfiable"
                     
 
mkFreeArgs :: [Type] -> Symbolic [Expr]
mkFreeArgs = mapM mkFreeArg

mkFreeArg :: Type -> Symbolic Expr
mkFreeArg BitT = BitE <$> free
mkFreeArg (BitsT w) = BitsE <$> freeBits w
mkFreeArg IntT = IntE <$> freeInt

deHoleStmts :: [Stmt] -> Symbolic [Stmt]
deHoleStmts = mapM deHoleStmt

deHoleStmt :: Stmt -> Symbolic Stmt
deHoleStmt (ReturnS x) = ReturnS <$> deHoleExpr x
deHoleStmt (DeclS ty nm e) = DeclS ty nm <$> deHoleExpr e
deHoleStmt (UpdateS nm e) = UpdateS nm <$> deHoleExpr e
deHoleStmt (ArrUpdateS nm idx e) = do
    idx' <- deHoleExpr idx
    e' <- deHoleExpr e
    return (ArrUpdateS nm idx' e')

deHoleExpr :: Expr -> Symbolic Expr
deHoleExpr (AndE a b) = do
    a' <- deHoleExpr a
    b' <- deHoleExpr b
    return (AndE a' b')
deHoleExpr (OrE a b) = do
    a' <- deHoleExpr a
    b' <- deHoleExpr b
    return (OrE a' b')
deHoleExpr (NotE a) = NotE <$> deHoleExpr a
deHoleExpr (HoleE ty) = mkFreeArg ty
deHoleExpr x@(BitE {}) = return x
deHoleExpr x@(BitsE {}) = return x
deHoleExpr x@(IntE {}) = return x
deHoleExpr x@(VarE {}) = return x
deHoleExpr (AccessE a b) = do
    a' <- deHoleExpr a
    b' <- deHoleExpr b
    return (AccessE a' b')

-- TODO: Cover more of the space of integers!
freeInt :: Symbolic Int
freeInt = msum (map return [0..10])


