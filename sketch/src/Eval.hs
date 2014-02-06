
module Eval (
    evalP, evalT,
    evalS, evalE, -- Exposed only for performance debugging
  ) where

import Smten.Prelude
import Smten.Data.Functor
import Smten.Data.Maybe
import qualified Smten.Data.Map as Map

import Bits
import EvalMonad
import Sketch

-- Evaluate a sketch program on the given program input.
--   It runs each harness on the harness's input, and returns whether all the
--   harnesses completed successfully.
evalP :: Prog -> ProgramInput -> Bool
evalP p i = all (evalD (envof p) i) p

-- Evaluate a type.
-- It should not fail.
evalT :: ProgEnv -> Type -> Type
evalT env BitT = BitT
evalT env (ArrT t e) = ArrT (evalT env t) $ ValE (fromJust $ runEvalM env (evalE e))
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
          in isJust (runEvalM env run)

-- Apply a function to the given arguments.
apply :: Function -> [Expr] -> EvalM Value
apply f xs = do
    xs' <- mapM evalE xs
    let args = Map.fromList (zip (f_args f) xs')
    scope args $ do
       evalS (f_body f)
       getOutput

-- | Evaluate a statement
evalS :: Stmt -> EvalM ()
evalS (ReturnS x) = {-# SCC "ReturnS" #-} do
  x' <- evalE x
  setOutput x'
evalS (AssertS p) = {-# SCC "AssertS" #-} do
  p' <- evalE p
  case p' of
    BitV b -> assert b
evalS (ReorderS {}) = {-# SCC "ReorderS" #-}
  -- Note: The generator inlines reorders, so we don't expect this case to ever
  -- occur.
  error "unexpected ReorderS during evaluation"
evalS (RepeatS {}) = {-# SCC "RepeatS" #-}
  -- Note: The generator inlines repeats, so we don't expect this case to ever
  -- occur.
  error "unexpected RepeatS during evaluation"
evalS (ForS init cond incr body) = {-# SCC "ForS" #-} do
  evalS init
  evalS $ WhileS cond (blockS [body, incr])
evalS w@(WhileS c s) = {-# SCC "WhileS" #-} do
  c' <- {-# SCC "WhileSCondition" #-} evalE c
  case c' of
    BitV False -> return ()
    BitV True -> do
      evalS s
      evalS w
evalS (DeclS ty nm) = {-# SCC "DeclS" #-} do
  v0 <- case ty of
          t@(ArrT {}) -> return $ pad t
          _ -> return (error $ nm ++ " not initialized")
  insertVar nm v0
evalS (UpdateS nm e) = {-# SCC "UpdateS" #-} do
  e' <- evalE e
  insertVar nm e'
evalS (ArrUpdateS nm i e) = {-# SCC "ArrUpdateS" #-} do
  mval <- lookupVar nm
  ir <- evalE i
  er <- evalE e
  arr' <- case (mval, ir, er) of
             (Just (BitsV xs), IntV i', BitV e') -> do
                assert (i' < length xs)
                return (BitsV $ arrupd xs i' e')
             (Just (ArrayV xs), IntV i', e') -> do
                assert (i' < length xs)
                return (ArrayV $ arrupd xs i' e')
  insertVar nm arr'

evalS (ArrBulkUpdateS nm lo _ e) = {-# SCC "ArrBulkUpdateS" #-} do
  -- Note: we don't have to evaluate the width argument, because the type of
  -- 'e' is already properly sized.
  mval <- lookupVar nm
  lor <- evalE lo
  er <- evalE e
  -- TODO: assert the indices for update are all in bounds
  arr' <- case (mval, lor, er) of
             (Just (BitsV xs), IntV lo', BitsV xs') -> do
                return (BitsV $ arrbulkupd xs lo' xs')
             (Just (ArrayV xs), IntV lo', ArrayV xs') -> do
                return (ArrayV $ arrbulkupd xs lo' xs')
  insertVar nm arr'

evalS (IfS p a b) = {-# SCC "IfS" #-} do
  p' <- evalE p
  case p' of
    BitV True -> evalS a
    BitV False -> evalS b
evalS (BlockS xs) = {-# SCC "BlockS" #-} mapM_ evalS xs

evalE :: Expr -> EvalM Value
evalE (ValE v) = {-# SCC "ValE" #-} return v
evalE (AndE a b) = {-# SCC "AndE" #-} do
    a' <- evalE a
    b' <- evalE b
    case (a', b') of
      (BitsV av, BitsV bv) -> return $ BitsV (av `andB` bv)
      (BitV av, BitV bv) -> return $ BitV (av && bv)
evalE (LAndE a b) = {-# SCC "LAndE" #-} do
    a' <- evalE a
    case a' of
        BitV True -> evalE b
        BitV False -> return a'
        BitsV av -> do  -- TODO: should we short circuit if av is 0?
            b' <- evalE b
            case b' of
                BitsV bv -> return $ BitsV (av `andB` bv)
evalE (OrE a b) = {-# SCC "OrE" #-} do
    a' <- evalE a
    b' <- evalE b
    case (a', b') of
      (BitsV av, BitsV bv) -> return $ BitsV (av `orB` bv)
      (BitV av, BitV bv) -> return $ BitV (av || bv)
evalE (LOrE a b) = {-# SCC "LOrE" #-} do
    a' <- evalE a
    case a' of
        BitV True -> return a'
        BitV False -> evalE b
        BitsV av -> do  -- TODO: should we short circuit if av is all ones?
            b' <- evalE b
            case b' of
                BitsV bv -> return $ BitsV (av `orB` bv)
evalE (AddE a b) = {-# SCC "AddE" #-} do
    a' <- evalE a
    b' <- evalE b
    case (a', b') of
      (BitsV av, BitsV bv) -> return $ BitsV (av `addB` bv)
      (IntV av, IntV bv) -> return $ IntV (av + bv)
evalE (SubE a b) = {-# SCC "SubE" #-} do
    a' <- evalE a
    b' <- evalE b
    case (a', b') of
      (BitsV av, BitsV bv) -> return $ BitsV (av `subB` bv)
      (IntV av, IntV bv) -> return $ IntV (av - bv)
evalE (LtE a b) = {-# SCC "LtE" #-} do
    a' <- evalE a
    b' <- evalE b
    case (a', b') of
      (IntV av, IntV bv) -> return $ BitV (av < bv)
evalE (GtE a b) = {-# SCC "GtE" #-} do
    a' <- evalE a
    b' <- evalE b
    case (a', b') of
      (IntV av, IntV bv) -> return $ BitV (av > bv)
evalE (LeE a b) = {-# SCC "LeE" #-} do
    a' <- evalE a
    b' <- evalE b
    case (a', b') of
      (IntV av, IntV bv) -> return $ BitV (av <= bv)
evalE (GeE a b) = {-# SCC "GeE" #-} do
    a' <- evalE a
    b' <- evalE b
    case (a', b') of
      (IntV av, IntV bv) -> return $ BitV (av >= bv)
evalE (EqE a b) = {-# SCC "EqE" #-} do
    a' <- evalE a
    b' <- evalE b
    case (a', b') of
      (BitV av, BitV bv) -> return $ BitV (av == bv)
      (BitsV av, BitsV bv) -> return $ BitV (av == bv)
      (IntV av, IntV bv) -> return $ BitV (av == bv)
evalE (NeqE a b) = {-# SCC "NeqE" #-} do
    a' <- evalE a
    b' <- evalE b
    case (a', b') of
      (BitV av, BitV bv) -> return $ BitV (av /= bv)
      (BitsV av, BitsV bv) -> return $ BitV (av /= bv)
      (IntV av, IntV bv) -> return $ BitV (av /= bv)
evalE (ArrayE xs) = {-# SCC "ArrayE" #-} arrayV <$> mapM evalE xs
evalE (NotE a) = {-# SCC "NotE" #-} do
    a' <- evalE a
    case a' of
      BitsV av -> return $ BitsV (notB av)
      BitV av -> return $ BitV (not av)
evalE (CondE p a b) = {-# SCC "CondE" #-} do
    p' <- evalE p
    case p' of
        BitV True -> evalE a
        BitV False -> evalE b
evalE (XorE a b) = {-# SCC "XorE" #-} do
    a' <- evalE a
    b' <- evalE b
    case (a', b') of
      (BitsV av, BitsV bv) -> return $ BitsV (av `xorB` bv)
      (BitV av, BitV bv) -> return $ BitV (av `xor` bv)
evalE (MulE a b) = {-# SCC "MulE" #-} do
    a' <- evalE a
    b' <- evalE b
    case (a', b') of
      (IntV av, IntV bv) -> return $ IntV (av * bv)
evalE (ModE a b) = {-# SCC "ModE" #-} do
    a' <- evalE a
    b' <- evalE b
    case (a', b') of
      (IntV av, IntV bv) -> do
         assert (bv /= 0)
         return $ IntV (av `rem` bv)
evalE (DivE a b) = {-# SCC "DivE" #-} do
    a' <- evalE a
    b' <- evalE b
    case (a', b') of
      (IntV av, IntV bv) -> do
         assert (bv /= 0)
         return $ IntV (av `quot` bv)
evalE (ShlE a b) = {-# SCC "ShlE" #-} do
    a' <- evalE a
    b' <- evalE b
    case (a', b') of
      (BitsV av, IntV bv) -> return $ BitsV (av `shlB` bv)
      (IntV av, IntV bv) -> return $ IntV (av `shlI` bv)
evalE (ShrE a b) = {-# SCC "ShrE" #-} do
    a' <- evalE a
    b' <- evalE b
    case (a', b') of
      (BitsV av, IntV bv) -> return $ BitsV (av `shrB` bv)
      (IntV av, IntV bv) -> return $ IntV (av `shrI` bv)
evalE (HoleE {}) = {-# SCC "HoleE" #-} error "HoleE in evalE"
evalE (VarE nm) = {-# SCC "VarE" #-} do
    mval <- lookupVar nm
    mdecl <- lookupDecl nm
    case (mval, mdecl) of
        (Just v, _) -> return v
        (_, Just d) -> evalE (d_val d)
        _ -> error $ "Variable " ++ show nm ++ " not found"
evalE (AccessE a i) = {-# SCC "AccessE" #-} do
    a' <- evalE a
    i' <- evalE i
    let idx = case i' of
               IntV iv -> iv
    case a' of
        BitsV av -> do
          assert (idx >= 0 && idx < length av)
          return (BitV (av !! idx))
        ArrayV xs -> do
          assert (idx >= 0 && idx < length xs)
          return (xs !! idx)
evalE (BulkAccessE a lo w) = {-# SCC "BulkAccessE" #-} do
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
evalE (CastE t e) = {-# SCC "CastE" #-} do
    e' <- evalE e
    case (t, e') of
        (IntT, BitsV v) -> return (IntV (valB v))
        (ArrT BitT (ValE (IntV w)), BitsV xs) -> return (BitsV (take w (xs ++ replicate w False)))
        (ArrT t (ValE (IntV w)), ArrayV xs) -> return (ArrayV (take w (xs ++ replicate w (pad t))))
        _ -> error $ "Unsupported cast of " ++ show e' ++ " to type " ++ show t
evalE (ICastE dst e) = {-# SCC "ICastE" #-} icast dst <$> evalE e
evalE (AppE f xs) = {-# SCC "AppE" #-} do
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

-- Implicitly cast the given value to the given type.
icast :: Type -> Value -> Value
icast dst v
  | dst == typeofV v = v
  | dimension dst > dimension (typeofV v) = icast dst (arrayV [v])
icast IntT (BitV False) = IntV 0
icast IntT (BitV True) = IntV 1
icast (ArrT BitT (ValE (IntV w))) (BitsV xs) = BitsV (take w (xs ++ replicate w False))
icast t@(ArrT {}) (BitsV xs) = icast t (ArrayV (map BitV xs))
icast (ArrT t (ValE (IntV w))) (ArrayV xs) = ArrayV $ take w (map (icast t) xs ++ replicate w (pad t))
icast t v = error $ "TODO: implement implicit cast of " ++ show v ++ " to " ++ show t

shlI :: Int -> Int -> Int
shlI x n
 | n < 0 = 0
 | n == 0 = x
 | otherwise = shlI (2*x) (n-1)

shrI :: Int -> Int -> Int
shrI x n
 | n < 0 = 0
 | n == 0 = x
 | otherwise = shrI (x `quot` 2) (n-1)

