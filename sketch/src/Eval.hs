
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}
module Eval (evalP) where

import Smten.Prelude
import Smten.Control.Monad.State
import qualified Smten.Data.Map as Map

import Bits
import Sketch

data SS = SS {
    -- | Local variables. Already evaluated.
    ss_vars :: Map.Map Name Expr,

    -- | The output of the statement if any.
    ss_out :: Expr
}

evalP :: [Stmt] -> Map.Map Name Expr -> Expr
evalP stmts args = ss_out $ execState (mapM evalS stmts) (SS args (error "evalP: output not defined"))

-- | Evaluate a statement
evalS :: Stmt -> State SS ()
evalS (ReturnS x) = do
  x' <- evalE x
  modify $ \s -> s { ss_out = x' }
evalS (DeclS _ nm e) = do
  e' <- evalE e
  modify $ \s -> s { ss_vars = Map.insert nm e' (ss_vars s) }
evalS (UpdateS nm e) = do
  e' <- evalE e
  modify $ \s -> s { ss_vars = Map.insert nm e' (ss_vars s) }
evalS (ArrUpdateS nm i e) = do
  env <- gets ss_vars
  ir <- evalE i
  er <- evalE e
  case (ir, er) of
     (IntE i', BitE e') -> do
          let arr' = case Map.lookup nm env of
                           Just (BitsE arr) -> updB arr i' e'
                           Just x -> error $ "array update: expected array of bits, but got: " ++ show x
                           Nothing -> error $ "array update: variable " ++ show nm ++ " not found"
          modify $ \s -> s { ss_vars = Map.insert nm (BitsE arr') (ss_vars s) }
     _ -> error $ "expecting: types int, bit for array update, but got: " ++ show (ir, er)

evalE :: Expr -> State SS Expr
evalE (AndE a b) = do
    a' <- evalE a
    b' <- evalE b
    case (a', b') of
      (BitsE av, BitsE bv) -> return $ BitsE (av `andB` bv)
      (BitE av, BitE bv) -> return $ BitE (av && bv)
      _ -> error $ "unexpected args to AndE: " ++ show (a', b')
evalE (OrE a b) = do
    a' <- evalE a
    b' <- evalE b
    case (a', b') of
      (BitsE av, BitsE bv) -> return $ BitsE (av `orB` bv)
      (BitE av, BitE bv) -> return $ BitE (av || bv)
evalE (HoleE {}) = error "HoleE in evalE"
evalE x@(BitE {}) = return x
evalE x@(BitsE {}) = return x
evalE x@(IntE {}) = return x
evalE (VarE nm) = do
    vars <- gets ss_vars
    case Map.lookup nm vars of
        Just v -> return v
        Nothing -> error $ "Var " ++ nm ++ " not in scope"
evalE (AccessE a i) = do
    BitsE a' <- evalE a
    IntE i' <- evalE i
    return (BitE (a' `accessB` i'))

