
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}
module Eval (evalP, evalT) where

import Smten.Prelude
import Smten.Control.Monad.State
import qualified Smten.Data.Map as Map

import Bits
import Sketch

type ProgramEnv = Map.Map String Decl

data SS = SS {
    -- | Local variables. Already evaluated.
    ss_vars :: Map.Map Name Expr,

    -- | The output of the statement if any.
    ss_out :: Expr,

    -- | Predicate which says if the result is valid.
    ss_valid :: Bool
}

-- Evaluate a sketch program on the given program input.
--   It runs each harness on the harness's input, and returns whether all the
--   harnesses completed successfully.
evalP :: Prog -> ProgramInput -> Bool
evalP p i =
  let env = Map.fromList [(d_name d, d) | d <- p]
  in all (evalD env i) p

-- Evaluate a type.
-- TODO: Don't we need to supply an environment here?
evalT :: Type -> Type
evalT BitT = BitT
evalT (BitsT e) = BitsT $ evalState (evalE e) (SS Map.empty (error "evalT.ss_out") True)
evalT IntT = IntT
evalT UnknownT = UnknownT

evalD :: ProgramEnv -> ProgramInput -> Decl -> Bool
evalD env i (VarD {}) = True
evalD env i d@(FunD {}) =
  case fd_spec d of
    Nothing -> True
    Just snm
      | Just sd@(FunD {}) <- Map.lookup snm env
      , Just args <- Map.lookup (d_name d) i ->
          let -- TODO: add global variables to the variable list   
              specargs = Map.fromList (zip (map snd (fd_args sd)) args)
              (want, gdw) = evalSs (fd_stmts sd) specargs
               
              sketchargs = Map.fromList (zip (map snd (fd_args d)) args)
              (got, gdg) = evalSs (fd_stmts d) sketchargs
          in and [gdw, gdg, want `valEq` got]

evalSs :: [Stmt] -> Map.Map Name Expr -> (Expr, Bool)
evalSs stmts args =
  case execState (mapM evalS stmts) (SS args (error "evalSs: output not defined") True) of
       SS _ e v -> (e, v)


assert :: Bool -> State SS ()
assert p = modify $ \s -> s { ss_valid = ss_valid s && p }

-- | Evaluate a statement
evalS :: Stmt -> State SS ()
evalS (ReturnS x) = do
  x' <- evalE x
  modify $ \s -> s { ss_out = x' }
evalS (AssertS p) = do
  p' <- evalE p
  case p' of
    BitE b -> assert b
    _ -> error $ "expected bit type for assert, but got: " ++ show p'
evalS (DeclS _ nm) =
  modify $ \s -> s { ss_vars = Map.insert nm (error $ nm ++ " not initialized") (ss_vars s) }
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
evalS (IfS p a b) = do
  p' <- evalE p
  case p' of
    BitE True -> evalS a
    BitE False -> evalS b
    _ -> error $ "expected bit type for if condition, but got: " ++ show p'
evalS (BlockS xs) = mapM_ evalS xs

evalE :: Expr -> State SS Expr
evalE (AndE a b) = do
    a' <- evalE a
    b' <- evalE b
    case (a', b') of
      (BitsE av, BitsE bv) -> return $ BitsE (av `andB` bv)
      (BitE av, BitE bv) -> return $ BitE (av && bv)
      _ -> error $ "unexpected args to AndE: " ++ show (a', b')
evalE (AddE a b) = do
    a' <- evalE a
    b' <- evalE b
    case (a', b') of
      (BitsE av, BitsE bv) -> return $ BitsE (av `addB` bv)
      _ -> error $ "unexpected args to AddE: " ++ show (a', b')
evalE (ArrayE xs) = do
    let f x = do
          r <- evalE x
          case r of
            BitE v -> return v
    bs <- mapM f xs
    return (BitsE (mkbits bs))
evalE (OrE a b) = do
    a' <- evalE a
    b' <- evalE b
    case (a', b') of
      (BitsE av, BitsE bv) -> return $ BitsE (av `orB` bv)
      (BitE av, BitE bv) -> return $ BitE (av || bv)
evalE (NotE a) = do
    a' <- evalE a
    case a' of
      BitsE av -> return $ BitsE (notB av)
      BitE av -> return $ BitE (not av)
evalE (XorE a b) = do
    a' <- evalE a
    b' <- evalE b
    case (a', b') of
      (BitsE av, BitsE bv) -> return $ BitsE (av `xorB` bv)
      (BitE av, BitE bv) -> return $ BitE (av `xor` bv)
evalE (MulE a b) = do
    a' <- evalE a
    b' <- evalE b
    case (a', b') of
      (IntE av, IntE bv) -> return $ IntE (av * bv)
evalE (ShlE a b) = do
    a' <- evalE a
    b' <- evalE b
    case (a', b') of
      (BitsE av, IntE bv) -> return $ BitsE (av `shlB` bv)
evalE (ShrE a b) = do
    a' <- evalE a
    b' <- evalE b
    case (a', b') of
      (BitsE av, IntE bv) -> return $ BitsE (av `shrB` bv)
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
    assert (i' < width a')
    return (BitE (a' `accessB` i'))

