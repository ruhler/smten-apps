
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

evalE :: Expr -> State SS Expr
evalE (AndE a b) = do
    BitsE a' <- evalE a
    BitsE b' <- evalE b
    return (BitsE (a' `andB` b'))
evalE (OrE a b) = do
    BitsE a' <- evalE a
    BitsE b' <- evalE b
    return (BitsE (a' `orB` b'))
evalE HoleE = error "HoleE in evalE"
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

