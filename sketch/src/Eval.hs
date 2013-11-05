
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}
module Eval (evalP, evalT) where

import Smten.Prelude
import Smten.Control.Monad.State
import Smten.Data.Functor
import qualified Smten.Data.Map as Map

import Bits
import Sketch

data SS = SS {
    -- | The Global Environment
    ss_env :: Map.Map Name Decl,

    -- | Local variables.
    ss_vars :: Map.Map Name Value,

    -- | The output of the statement if any.
    ss_out :: Value,

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
evalT env (ArrT t e) = ArrT (evalT env t) $ ValE (evalState (evalE e) (SS env Map.empty (error "evalT.ss_out") True))
evalT env IntT = IntT
evalT env (FunT x xs) = FunT (evalT env x) (map (evalT env) xs)
evalT env UnknownT = UnknownT

evalD :: ProgEnv -> ProgramInput -> Decl -> Bool
evalD env i (VarD {}) = True
evalD env i d@(FunD {}) =
  case fd_kind d of
    NormalF -> True
    GeneratorF -> True
    WithSpecF snm
      | Just sd@(FunD {}) <- Map.lookup snm env
      , Just args <- map ValE <$> Map.lookup (d_name d) i ->
          let run = do
                want <- apply (fd_val sd) args
                got <- apply (fd_val d) args
                assert (want == got)
          in ss_valid $ execState run (SS env Map.empty (error "evalD: no output produces") True)

-- Apply a function to the given arguments.
apply :: Function -> [Expr] -> State SS Value
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
    BitV b -> assert b
    _ -> error $ "expected bit type for assert, but got: " ++ show p'
evalS (RepeatS n s) = do
  -- Note: The generator inlines repeats, so we don't expect this case to ever
  -- occur.
  n' <- evalE n
  case n' of
    IntV nv -> mapM_ evalS (replicate nv s)
    _ -> error $ "expected int type for repeat count, but got: " ++ show n'
evalS (ForS init cond incr body) = do
  evalS init
  evalS $ WhileS cond (blockS [body, incr])
evalS w@(WhileS c s) = do
  c' <- evalE c
  case c' of
    BitV False -> return ()
    BitV True -> do
      evalS s
      evalS w
    _ -> error $ "expected bit type for while condition, but got: " ++ show c'
evalS (DeclS ty nm) = do
  v0 <- case ty of
          t@(ArrT {}) -> return $ pad t
          _ -> return (error $ nm ++ " not initialized")
  modify $ \s -> s { ss_vars = Map.insert nm v0 (ss_vars s) }
evalS (UpdateS nm e) = do
  e' <- evalE e
  modify $ \s -> s { ss_vars = Map.insert nm e' (ss_vars s) }
evalS (ArrUpdateS nm i e) = do
  env <- gets ss_vars
  ir <- asint <$> evalE i
  er <- evalE e
  arr' <- case (Map.lookup nm env, ir, er) of
             (Just (BitsV arr), IntV i', BitV e') -> do
                assert (i' < width arr)
                return (BitsV $ updB arr i' e')
             (Just (ArrayV xs), IntV i', e') -> do
                 let f _ [] = error "array update out of bounds"
                     f 0 (v:vs) = e':vs
                     f n (v:vs) = v : f (n-1) vs
                 assert (i' < length xs)
                 return (ArrayV $ f i' xs)
             (Just v, _, _) -> error $ "array update into non-array: " ++ show v
             (Nothing, _, _) -> error $ "array update: variable " ++ show nm ++ " not found"
  modify $ \s -> s { ss_vars = Map.insert nm arr' (ss_vars s) }

evalS (IfS p a b) = do
  p' <- evalE p
  case p' of
    BitV True -> evalS a
    BitV False -> evalS b
    _ -> error $ "expected bit type for if condition, but got: " ++ show p'
evalS (BlockS xs) = mapM_ evalS xs

evalE :: Expr -> State SS Value
evalE (ValE v) = return v
evalE (AndE a b) = do
    a' <- evalE a
    b' <- evalE b
    case (a', b') of
      (BitsV av, BitsV bv) -> return $ BitsV (av `andB` bv)
      (BitV av, BitV bv) -> return $ BitV (av && bv)
      _ -> error $ "unexpected args to AndE: " ++ show (a', b')
evalE (AddE a b) = do
    a' <- evalE a
    b' <- evalE b
    case (a', b') of
      (BitsV av, BitsV bv) -> return $ BitsV (av `addB` bv)
      (IntV av, IntV bv) -> return $ IntV (av + bv)
      _ -> error $ "unexpected args to AddE: " ++ show (a', b')
evalE (SubE a b) = do
    a' <- evalE a
    b' <- evalE b
    case (a', b') of
      (BitsV av, BitsV bv) -> return $ BitsV (av `subB` bv)
      (IntV av, IntV bv) -> return $ IntV (av - bv)
      _ -> error $ "unexpected args to SubE: " ++ show (a', b')
evalE (LtE a b) = do
    a' <- asint <$> evalE a
    b' <- asint <$> evalE b
    case (a', b') of
      (IntV av, IntV bv) -> return $ BitV (av < bv)
      _ -> error $ "unexpected args to LtE: " ++ show (a', b')
evalE (GtE a b) = do
    a' <- asint <$> evalE a
    b' <- asint <$> evalE b
    case (a', b') of
      (IntV av, IntV bv) -> return $ BitV (av > bv)
      _ -> error $ "unexpected args to GtE: " ++ show (a', b')
evalE (LeE a b) = do
    a' <- asint <$> evalE a
    b' <- asint <$> evalE b
    case (a', b') of
      (IntV av, IntV bv) -> return $ BitV (av <= bv)
      _ -> error $ "unexpected args to LeE: " ++ show (a', b')
evalE (GeE a b) = do
    a' <- asint <$> evalE a
    b' <- asint <$> evalE b
    case (a', b') of
      (IntV av, IntV bv) -> return $ BitV (av >= bv)
      _ -> error $ "unexpected args to GeE: " ++ show (a', b')
evalE (EqE a b) = do
    a' <- evalE a
    b' <- evalE b
    case (a', b') of
      (BitV av, BitV bv) -> return $ BitV (av == bv)
      (BitsV av, BitsV bv) -> return $ BitV (av `eqB` bv)
      (IntV av, IntV bv) -> return $ BitV (av == bv)
      _ -> error $ "unexpected args to EqE: " ++ show (a', b')
evalE (NeqE a b) = do
    a' <- evalE a
    b' <- evalE b
    case (a', b') of
      (BitV av, BitV bv) -> return $ BitV (av /= bv)
      (BitsV av, BitsV bv) -> return $ BitV (av `neqB` bv)
      (IntV av, IntV bv) -> return $ BitV (av /= bv)
      _ -> error $ "unexpected args to NeqE: " ++ show (a', b')
evalE (ArrayE xs) = arrayV <$> mapM evalE xs
evalE (OrE a b) = do
    a' <- evalE a
    b' <- evalE b
    case (a', b') of
      (BitsV av, BitsV bv) -> return $ BitsV (av `orB` bv)
      (BitV av, BitV bv) -> return $ BitV (av || bv)
evalE (LOrE a b) = do
    a' <- evalE a
    case a' of
        BitV True -> return a'
        BitV False -> evalE b
        _ -> error $ "unexpected first argument to logical or: " ++ show a'
evalE (LAndE a b) = do
    a' <- evalE a
    case a' of
        BitV True -> evalE b
        BitV False -> return a'
        _ -> error $ "unexpected first argument to logical and: " ++ show a'
evalE (NotE a) = do
    a' <- evalE a
    case a' of
      BitsV av -> return $ BitsV (notB av)
      BitV av -> return $ BitV (not av)
evalE (XorE a b) = do
    a' <- evalE a
    b' <- evalE b
    case (a', b') of
      (BitsV av, BitsV bv) -> return $ BitsV (av `xorB` bv)
      (BitV av, BitV bv) -> return $ BitV (av `xor` bv)
      _ -> error $ "unexpected args to XorE: " ++ show (a', b')
evalE (MulE a b) = do
    a' <- evalE a
    b' <- evalE b
    case (a', b') of
      (IntV av, IntV bv) -> return $ IntV (av * bv)
evalE (ModE a b) = do
    a' <- evalE a
    b' <- evalE b
    case (a', b') of
      (IntV av, IntV bv) -> do
         assert (bv /= 0)
         return $ IntV (av `rem` bv)
evalE (ShlE a b) = do
    a' <- evalE a
    b' <- evalE b
    case (a', b') of
      (BitsV av, IntV bv) -> return $ BitsV (av `shlB` bv)
evalE (ShrE a b) = do
    a' <- evalE a
    b' <- evalE b
    case (a', b') of
      (BitsV av, IntV bv) -> return $ BitsV (av `shrB` bv)
evalE (HoleE {}) = error "HoleE in evalE"
evalE (VarE nm) = do
    vars <- gets ss_vars
    case Map.lookup nm vars of
        Just v -> return v
        Nothing -> do
            env <- gets ss_env
            case Map.lookup nm env of
                Just (VarD _ _ v) -> evalE v
                Just (FunD _ v _) -> return (FunV v)
                Nothing -> error $ "Var " ++ nm ++ " not in scope"
evalE (AccessE a i) = do
    a' <- evalE a
    i' <- asint <$> evalE i
    let idx = case i' of
               IntV iv -> iv
    case a' of
        BitsV av -> do
          assert (idx < width av)
          return (BitV (av `accessB` idx))
        ArrayV xs -> do
          assert (idx < length xs)
          return (xs !! idx)
evalE (CastE t e) = do
    e' <- evalE e
    case (t, e') of
        (IntT, BitsV v) -> return (IntV (valB v))
        (ArrT BitT (ValE (IntV w)), BitsV v) -> return (BitsV (castB w v))
        (ArrT t (ValE (IntV w)), ArrayV xs) -> return (ArrayV (take w (xs ++ replicate w (pad t))))
        _ -> error $ "Unsupported cast of " ++ show e' ++ " to type " ++ show t
evalE (ICastE src dst e) = do
    e' <- evalE e
    case (dst, e') of
       (IntT, BitV False) -> return $ IntV 0
       (IntT, BitV True) -> return $ IntV 1
       (ArrT BitT (ValE (IntV w)), BitsV v) -> return (BitsV (castB w v))
       (ArrT t (ValE (IntV w)), ArrayV xs) -> return (ArrayV (take w (xs ++ replicate w (pad t))))
       _ -> error $ "TODO: implement implicit cast of " ++ show e' ++ " to " ++ show dst
evalE (AppE f xs) = do
    f' <- evalE (VarE f)
    case f' of
        FunV fv -> apply fv xs
        _ -> error $ "Expected function, but got: " ++ show f'
    
    
pad :: Type -> Value
pad BitT = BitV False
pad IntT = IntV 0
pad (ArrT t (ValE (IntV w))) = arrayV (replicate w (pad t))

-- promote to int as needed
-- It's an error if the value can't be implicitly converted to an Int.
asint :: Value -> Value
asint (BitV False) = IntV 0
asint (BitV True) = IntV 1
asint x@(IntV {})  = x
asint x = error $ "cannot implicitly convert to int: " ++ show x

dimension :: Type -> Int
dimension BitT = 1
dimension (ArrT t _) = 1 + dimension t
dimension IntT = 1
dimension (FunT {}) = 1

