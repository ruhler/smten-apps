
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}
module Eval (evalP, evalT) where

import Smten.Prelude
import Smten.Control.Monad.State
import qualified Smten.Data.Map as Map

import Bits
import Sketch

data SS = SS {
    -- | The Global Environment
    ss_env :: Map.Map Name Decl,

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
evalP p i = all (evalD (envof p) i) p

-- Evaluate a type.
evalT :: ProgEnv -> Type -> Type
evalT env BitT = BitT
evalT env (ArrT t e) = ArrT (evalT env t) $ evalState (evalE e) (SS env Map.empty (error "evalT.ss_out") True)
evalT env IntT = IntT
evalT env UnknownT = UnknownT

evalD :: ProgEnv -> ProgramInput -> Decl -> Bool
evalD env i (VarD {}) = True
evalD env i d@(FunD {}) =
  case fd_kind d of
    NormalF -> True
    GeneratorF -> True
    WithSpecF snm
      | Just sd@(FunD {}) <- Map.lookup snm env
      , Just args <- Map.lookup (d_name d) i ->
          let run = do
                want <- apply (fd_val sd) args
                got <- apply (fd_val d) args
                assert (want `valEq` got)
          in ss_valid $ execState run (SS env Map.empty (error "evalD: no output produces") True)

-- Apply a function to the given arguments.
apply :: Function -> [Expr] -> State SS Expr
apply f xs = do
    xs' <- mapM evalE xs
    let args = Map.fromList (zip (f_args f) xs')
    olds <- get
    put (SS (ss_env olds) args (error "apply: no output returned") (ss_valid olds))
    evalS (f_body f)
    r <- gets ss_out
    p <- gets ss_valid
    modify $ \s -> s { ss_out = ss_out olds, ss_vars = ss_vars olds }
    return r

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
evalS (RepeatS n s) = do
  n' <- evalE n
  case n' of
    IntE nv -> mapM_ evalS (replicate nv s)
    _ -> error $ "expected int type for repeat count, but got: " ++ show n'
evalS (ForS init cond incr body) = do
  evalS init
  evalS $ WhileS cond (BlockS [body, incr])
evalS w@(WhileS c s) = do
  c' <- evalE c
  case c' of
    BitE False -> return ()
    BitE True -> do
      evalS s
      evalS w
    _ -> error $ "expected bit type for while condition, but got: " ++ show c'
evalS (DeclS ty nm) = do
  env <- gets ss_env
  v0 <- case evalT env ty of
          t@(ArrT {}) -> evalE $ pad ty
          _ -> return (error $ nm ++ " not initialized")
  modify $ \s -> s { ss_vars = Map.insert nm v0 (ss_vars s) }
evalS (UpdateS nm e) = do
  e' <- evalE e
  modify $ \s -> s { ss_vars = Map.insert nm e' (ss_vars s) }
evalS (ArrUpdateS nm i e) = do
  env <- gets ss_vars
  ir <- evalE i
  er <- evalE e
  arr' <- case (Map.lookup nm env, ir, er) of
             (Just (BitsE arr), IntE i', BitE e') -> do
                assert (i' < width arr)
                return (BitsE $ updB arr i' e')
             (Just (ArrayE xs), IntE i', e') -> do
                 let f _ [] = error "array update out of bounds"
                     f 0 (v:vs) = e':vs
                     f n (v:vs) = v : f (n-1) vs
                 assert (i' < length xs)
                 return (ArrayE $ f i' xs)
             (Just v, _, _) -> error $ "array update into non-array: " ++ show v
             (Nothing, _, _) -> error $ "array update: variable " ++ show nm ++ " not found"
  modify $ \s -> s { ss_vars = Map.insert nm arr' (ss_vars s) }

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
      (IntE av, IntE bv) -> return $ IntE (av + bv)
      _ -> error $ "unexpected args to AddE: " ++ show (a', b')
evalE (SubE a b) = do
    a' <- evalE a
    b' <- evalE b
    case (a', b') of
      (BitsE av, BitsE bv) -> return $ BitsE (av `subB` bv)
      (IntE av, IntE bv) -> return $ IntE (av - bv)
      _ -> error $ "unexpected args to SubE: " ++ show (a', b')
evalE (LtE a b) = do
    a' <- evalE a
    b' <- evalE b
    case (a', b') of
      (BitsE av, BitsE bv) -> return $ BitE (av `ltB` bv)
      (IntE av, IntE bv) -> return $ BitE (av < bv)
      _ -> error $ "unexpected args to LtE: " ++ show (a', b')
evalE (GtE a b) = do
    a' <- evalE a
    b' <- evalE b
    case (a', b') of
      (BitsE av, BitsE bv) -> return $ BitE (av `gtB` bv)
      (IntE av, IntE bv) -> return $ BitE (av > bv)
      _ -> error $ "unexpected args to GtE: " ++ show (a', b')
evalE (EqE a b) = do
    a' <- evalE a
    b' <- evalE b
    case (a', b') of
      (BitE av, BitE bv) -> return $ BitE (av == bv)
      (BitsE av, BitsE bv) -> return $ BitE (av `eqB` bv)
      (IntE av, IntE bv) -> return $ BitE (av == bv)
      _ -> error $ "unexpected args to EqE: " ++ show (a', b')
evalE x@(ArrayE []) = return x
evalE (ArrayE xs) = do
    xs' <- mapM evalE xs
    case head xs' of
        BitE _ -> return $ BitsE (mkbits [v | BitE v <- xs'])
        _ -> return $ ArrayE xs'
evalE (OrE a b) = do
    a' <- evalE a
    b' <- evalE b
    case (a', b') of
      (BitsE av, BitsE bv) -> return $ BitsE (av `orB` bv)
      (BitE av, BitE bv) -> return $ BitE (av || bv)
evalE (LOrE a b) = do
    a' <- evalE a
    case a' of
        BitE True -> return a'
        BitE False -> evalE b
        _ -> error $ "unexpected first argument to logical or: " ++ show a'
evalE (LAndE a b) = do
    a' <- evalE a
    case a' of
        BitE True -> evalE b
        BitE False -> return a'
        _ -> error $ "unexpected first argument to logical and: " ++ show a'
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
      _ -> error $ "unexpected args to XorE: " ++ show (a', b')
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
        Nothing -> do
            env <- gets ss_env
            case Map.lookup nm env of
                Just (VarD _ _ v) -> return v
                Just (FunD _ v _) -> return (FunE v)
                Nothing -> error $ "Var " ++ nm ++ " not in scope"
evalE (AccessE a i) = do
    a' <- evalE a
    i' <- evalE i
    let idx = case i' of
               BitE False -> 0
               BitE True -> 1
               BitsE b -> valB b
               IntE iv -> iv
    case a' of
        BitsE av -> do
          assert (idx < width av)
          return (BitE (av `accessB` idx))
        ArrayE xs -> do
          assert (idx < length xs)
          return (xs !! idx)
evalE (CastE t e) = do
    env <- gets ss_env
    e' <- evalE e
    case (evalT env t, e') of
        (IntT, BitsE v) -> return (IntE (valB v))
        (ArrT BitT (IntE w), BitsE v) -> return (BitsE (castB w v))
        _ -> error $ "Unsupported cast of " ++ show e' ++ " to type " ++ show t
evalE x@(FunE {}) = return x
evalE (AppE f xs) = do
    f' <- evalE (VarE f)
    case f' of
        FunE fv -> apply fv xs
        _ -> error $ "Expected function, but got: " ++ show f'
    
    
pad :: Type -> Expr
pad BitT = BitE False
pad IntT = IntE 0
pad (ArrT t (IntE w)) = ArrayE (replicate w (pad t))

