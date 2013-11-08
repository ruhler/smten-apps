
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
  ir <- evalE i
  er <- evalE e
  arr' <- case (Map.lookup nm env, ir, er) of
             (Just (BitsV xs), IntV i', BitV e') -> do
                assert (i' < length xs)
                return (BitsV $ arrupd xs i' e')
             (Just (ArrayV xs), IntV i', e') -> do
                 assert (i' < length xs)
                 return (ArrayV $ arrupd xs i' e')
             (Just v, _, _) -> error $ "array update into non-array: " ++ show v
             (Nothing, _, _) -> error $ "array update: variable " ++ show nm ++ " not found"
  modify $ \s -> s { ss_vars = Map.insert nm arr' (ss_vars s) }

evalS (ArrBulkUpdateS nm lo _ e) = do
  -- Note: we don't have to evaluate the width argument, because the type of
  -- 'e' is already properly sized.
  env <- gets ss_vars
  lor <- evalE lo
  er <- evalE e
  -- TODO: assert the indices for update are all in bounds
  arr' <- case (Map.lookup nm env, lor, er) of
             (Just (BitsV xs), IntV lo', BitsV xs') -> do
                return (BitsV $ arrbulkupd xs lo' xs')
             (Just (ArrayV xs), IntV lo', ArrayV xs') -> do
                return (ArrayV $ arrbulkupd xs lo' xs')
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
    a' <- evalE a
    b' <- evalE b
    case (a', b') of
      (IntV av, IntV bv) -> return $ BitV (av < bv)
      _ -> error $ "unexpected args to LtE: " ++ show (a', b')
evalE (GtE a b) = do
    a' <- evalE a
    b' <- evalE b
    case (a', b') of
      (IntV av, IntV bv) -> return $ BitV (av > bv)
      _ -> error $ "unexpected args to GtE: " ++ show (a', b')
evalE (LeE a b) = do
    a' <- evalE a
    b' <- evalE b
    case (a', b') of
      (IntV av, IntV bv) -> return $ BitV (av <= bv)
      _ -> error $ "unexpected args to LeE: " ++ show (a', b')
evalE (GeE a b) = do
    a' <- evalE a
    b' <- evalE b
    case (a', b') of
      (IntV av, IntV bv) -> return $ BitV (av >= bv)
      _ -> error $ "unexpected args to GeE: " ++ show (a', b')
evalE (EqE a b) = do
    a' <- evalE a
    b' <- evalE b
    case (a', b') of
      (BitV av, BitV bv) -> return $ BitV (av == bv)
      (BitsV av, BitsV bv) -> return $ BitV (av == bv)
      (IntV av, IntV bv) -> return $ BitV (av == bv)
      _ -> error $ "unexpected args to EqE: " ++ show (a', b')
evalE (NeqE a b) = do
    a' <- evalE a
    b' <- evalE b
    case (a', b') of
      (BitV av, BitV bv) -> return $ BitV (av /= bv)
      (BitsV av, BitsV bv) -> return $ BitV (av /= bv)
      (IntV av, IntV bv) -> return $ BitV (av /= bv)
      _ -> error $ "unexpected args to NeqE: " ++ show (a', b')
evalE (ArrayE xs) = arrayV <$> mapM evalE xs
evalE (OrE a b) = do
    a' <- evalE a
    b' <- evalE b
    case (a', b') of
      (BitsV av, BitsV bv) -> return $ BitsV (av `orB` bv)
      (BitV av, BitV bv) -> return $ BitV (av || bv)
evalE (NotE a) = do
    a' <- evalE a
    case a' of
      BitsV av -> return $ BitsV (notB av)
      BitV av -> return $ BitV (not av)
evalE (CondE p a b) = do
    p' <- evalE p
    case p' of
        BitV True -> evalE a
        BitV False -> evalE b
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
evalE (DivE a b) = do
    a' <- evalE a
    b' <- evalE b
    case (a', b') of
      (IntV av, IntV bv) -> do
         assert (bv /= 0)
         return $ IntV (av `quot` bv)
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
    i' <- evalE i
    let idx = case i' of
               IntV iv -> iv
    case a' of
        BitsV av -> do
          assert (idx < length av)
          return (BitV (av !! idx))
        ArrayV xs -> do
          assert (idx < length xs)
          return (xs !! idx)
evalE (BulkAccessE a lo w) = do
    a' <- evalE a
    IntV lo' <- evalE lo
    IntV w' <- evalE w
    case a' of
        BitsV xs -> do
          let xs' = drop lo' xs
          assert (lo' >= 0 && w' <= length xs')
          return (BitsV (take w' xs'))
        ArrayV xs -> do
          let xs' = drop lo' xs
          assert (lo' >= 0 && w' <= length xs')
          return (ArrayV (take w' xs'))
evalE (CastE t e) = do
    e' <- evalE e
    case (t, e') of
        (IntT, BitsV v) -> return (IntV (valB v))
        (ArrT BitT (ValE (IntV w)), BitsV xs) -> return (BitsV (take w (xs ++ replicate w False)))
        (ArrT t (ValE (IntV w)), ArrayV xs) -> return (ArrayV (take w (xs ++ replicate w (pad t))))
        _ -> error $ "Unsupported cast of " ++ show e' ++ " to type " ++ show t
evalE (ICastE src dst e) = do
    e' <- evalE e
    case (dst, e') of
       (IntT, BitV False) -> return $ IntV 0
       (IntT, BitV True) -> return $ IntV 1
       (ArrT BitT (ValE (IntV w)), BitsV xs) -> return (BitsV (take w (xs ++ replicate w False)))
       _ | dimension dst > dimension (typeofV e') -> do
           evalE (ICastE src dst (ArrayE [ValE e']))
       (ArrT t (ValE (IntV w)), ArrayV xs) -> return (ArrayV (take w (xs ++ replicate w (pad t))))
       _ -> error $ "TODO: implement implicit cast of " ++ show e' ++ " to " ++ show dst
evalE (AppE f xs) = do
    f' <- evalE (VarE f)
    case f' of
        FunV fv -> apply fv xs
        _ -> error $ "Expected function, but got: " ++ show f'
    
    
-- Update the ith element of an array
arrupd :: [a] -> Int -> a -> [a]
arrupd [] _ _ = error "arrupd: update out of bounds"
arrupd (x:xs) 0 v = v : xs
arrupd (x:xs) n v = x : arrupd xs (n-1) v

-- Do a bulk update starting at the given index.
arrbulkupd :: [a] -> Int -> [a] -> [a]
arrbulkupd vals i vals' =
  let lo = take i vals
      mid = vals'
      hi = drop (i + length vals') vals
  in concat [lo, mid, hi]

