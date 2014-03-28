
module Eval (
    evalP, evalT,
    evalS, evalE, -- Exposed only for performance debugging
  ) where

import Smten.Prelude
import Smten.Control.Monad
import Smten.Data.Functor
import Smten.Data.Maybe
import qualified Smten.Data.Map as Map

import Bits
import EvalMonad
import Input
import Program
import Syntax

-- Evaluate a sketch program on the given program input.
--   It runs each harness on the harness's input, and returns whether all the
--   harnesses completed successfully.
evalP :: Program -> ProgramInput -> Bool
evalP p i = all (evalD p i) (decls p)

-- Evaluate a type.
-- It should not fail.
evalT :: Program -> Type -> Type
evalT env VoidT = VoidT
evalT env BitT = BitT
evalT env (ArrT t e) = ArrT (evalT env t) $ ValE (fromJust $ runEvalM env (evalE e))
evalT env IntT = IntT
evalT env (FunT x xs) = FunT (evalT env x) (map (evalT env) xs)
evalT env UnknownT = UnknownT

evalD :: Program -> ProgramInput -> Decl -> Bool
evalD env i (VarD {}) = True
evalD env i d@(FunD {}) =
  case fd_kind d of
    NormalF -> True
    GeneratorF -> True
    HarnessF
      | Just args <- map ValE <$> Map.lookup (declN d) i ->
          isJust (runEvalM env (apply (fd_val d) args))
    WithSpecF snm
      | Just sd@(FunD {}) <- Map.lookup snm env
      , Just args <- map ValE <$> Map.lookup (declN d) i ->
          let run = do
                want <- apply (fd_val sd) args
                got <- apply (fd_val d) args
                assert (want == got)
          in isJust (runEvalM env run)

-- Apply a function to the given arguments.
apply :: Function -> [Expr] -> EvalM Value
apply f xs = do
    xs' <- mapM evalE xs
    let args = Map.fromList (zip [nm | Arg nm _ _ <- f_args f] xs')
    (v, args') <- scope args $ returned <$> evalS (f_body f)

    let updref :: Arg -> Expr -> EvalM ()
        updref (Arg _ _ False) _ = return ()
        updref (Arg nm _ True) x = do
           let lv = fromJust $ asLVal x
               v = fromJust $ Map.lookup nm args'
           updateLV lv v
    zipWithM_ updref (f_args f) xs
    return v

data StmtResult = OK | RET Value

returned :: StmtResult -> Value
returned OK = VoidV
returned (RET v) = v

-- | Evaluate a statement
evalS :: Stmt -> EvalM StmtResult
evalS (ReturnS x) = {-# SCC "ReturnS" #-} RET <$> evalE x
evalS (ExprS x) = {-# SCC "ExprS" #-} const OK <$> evalE x
evalS (AssertS p) = {-# SCC "AssertS" #-} do
  p' <- evalE p
  case p' of
    BitV b -> do
        assert b
        return OK
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
    BitV False -> return OK
    BitV True -> do
      evalS s   -- TODO: if this statement returns, should we break out of the loop?
      evalS w
evalS (DeclS ty vars) = {-# SCC "DeclS" #-} do
  let f (nm, mval) = do
          let defv = case ty of
                        t@(ArrT {}) -> pad t
                        _ -> error $ "variable " ++ nm ++ " uninitialized"
          v0 <- evalE $ fromMaybe (ValE defv) mval
          insertVar nm v0
  mapM_ f vars
  return OK
evalS (UpdateS lv e) = {-# SCC "UpdateS" #-} do
  e' <- evalE e
  updateLV lv e'
  return OK

evalS (IfS p a b) = {-# SCC "IfS" #-} do
  p' <- evalE p
  case p' of
    BitV True -> evalS a
    BitV False -> evalS b

evalS (BlockS []) = {-# SCC "BlockS" #-} return OK
evalS (BlockS (x:xs)) = {-# SCC "BlockS" #-} do
    r <- evalS x
    case r of
       OK -> evalS (BlockS xs)
       RET v -> return (RET v)

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
evalE (AddE a b) = liftM2 (+) (evalE a) (evalE b)
evalE (SubE a b) = liftM2 (-) (evalE a) (evalE b)
evalE (LtE a b) = BitV <$> liftM2 (<) (evalE a) (evalE b)
evalE (GtE a b) = BitV <$> liftM2 (>) (evalE a) (evalE b)
evalE (LeE a b) = BitV <$> liftM2 (<=) (evalE a) (evalE b)
evalE (GeE a b) = BitV <$> liftM2 (>=) (evalE a) (evalE b)
evalE (EqE a b) = BitV <$> liftM2 (==) (evalE a) (evalE b)
evalE (NeqE a b) = BitV <$> liftM2 (/=) (evalE a) (evalE b)
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
evalE (MulE a b) = liftM2 (*) (evalE a) (evalE b)
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
evalE (PostIncrE lv) = {-# SCC "PostIncrE" #-} do
    a' <- lookupLV lv
    updateLV lv (a' + IntV 1)
    case a' of
        IntV av -> updateLV lv (IntV (av + 1))
    return a'
evalE (PostDecrE lv) = {-# SCC "PostDecrE" #-} do
    a' <- lookupLV lv
    updateLV lv (a' - IntV 1)
    return a'
evalE (PreIncrE lv) = {-# SCC "PreIncrE" #-} do
    a <- lookupLV lv
    let a' = a + IntV 1
    updateLV lv a'
    return a'
evalE (PreDecrE lv) = {-# SCC "PreDecrE" #-} do
    a <- lookupLV lv
    let a' = a - IntV 1
    updateLV lv a'
    return a'
evalE (PlusEqE lv x) = {-# SCC "PlusEqE" #-} do
    a <- lookupLV lv
    x' <- evalE x
    let a' = a + x'
    updateLV lv a'
    return a'
evalE (HoleE {}) = {-# SCC "HoleE" #-} error "HoleE in evalE"
evalE (VarE nm) = {-# SCC "VarE" #-} do
    mval <- lookupVar nm
    mdecl <- lookupDecl nm
    case (mval, mdecl) of
        (Just v, _) -> return v
        (_, Just d) -> evalE (declE d)
        _ -> error $ "Variable " ++ show nm ++ " not found"
evalE (AccessE a i) = {-# SCC "AccessE" #-} do
    a' <- evalE a
    arrayAccess a' i
evalE (BulkAccessE a lo w) = {-# SCC "BulkAccessE" #-} do
    a' <- evalE a
    bulkAccess a' lo w
evalE (CastE t e) = {-# SCC "CastE" #-} do
    e' <- evalE e
    case (t, e') of
        (IntT, BitV v) -> return (IntV (if v then 1 else 0))
        (IntT, BitsV v) -> return (IntV (valB v))
        (ArrT BitT (ValE (IntV w)), BitV x) -> return (BitsV (x : replicate (w-1) False))
        (ArrT BitT (ValE (IntV w)), BitsV xs) -> return (BitsV (take w (xs ++ replicate w False)))
        (ArrT t (ValE (IntV w)), ArrayV xs) -> return (ArrayV (take w (xs ++ replicate w (pad t))))
        _ -> error $ "Unsupported cast of " ++ show e' ++ " to type " ++ show t
evalE (ICastE dst e) = {-# SCC "ICastE" #-} icast dst <$> evalE e
evalE (AppE f xs) = {-# SCC "AppE" #-} do
    f' <- evalE (VarE f)
    case f' of
        FunV fv -> apply fv xs
        _ -> error $ "Expected function, but got: " ++ show f'

-- Given an array value and index, return the value at that index.
-- TODO: make an LValE, have it use lookupLV, then inline this into lookupLV.
arrayAccess :: Value -> Expr -> EvalM Value
arrayAccess arr i = do
    i' <- evalE i
    let idx = case i' of
               IntV iv -> iv
    case arr of
        BitsV av -> do
          assert (idx >= 0 && idx < length av)
          return (BitV (arrsub av idx))
        ArrayV xs -> do
          assert (idx >= 0 && idx < length xs)
          return (arrsub xs idx)

bulkAccess :: Value -> Expr -> Expr -> EvalM Value
bulkAccess arr lo w = do
    IntV lo' <- evalE lo
    IntV w' <- evalE w
    case arr of
        BitsV xs -> do
          let xs' = drop lo' xs
          assert (lo' >= 0 && w' <= length xs')
          return (BitsV (take w' xs'))
        ArrayV xs -> do
          let xs' = drop lo' xs
          assert (lo' >= 0 && w' <= length xs')
          return (ArrayV (take w' xs'))
    
lookupLV :: LVal -> EvalM Value
lookupLV (VarLV nm) = fromJust <$> lookupVar nm
lookupLV (ArrLV lv i) = do
    arr <- lookupLV lv
    arrayAccess arr i
lookupLV (BulkLV lv lo w) = do
    arr <- lookupLV lv
    bulkAccess arr lo w

updateLV :: LVal -> Value -> EvalM ()
updateLV (VarLV nm) x = insertVar nm x
updateLV (ArrLV lv i) x = do
  arr <- lookupLV lv
  ir <- evalE i
  arr' <- case (arr, ir, x) of
             (BitsV xs, IntV i', BitV e') -> do
                assert (i' < length xs)
                return (BitsV $ arrupd xs i' e')
             (ArrayV xs, IntV i', e') -> do
                assert (i' < length xs)
                return (ArrayV $ arrupd xs i' e')
  updateLV lv arr'
updateLV (BulkLV lv lo _) x = do
  -- Note: we don't have to evaluate the width argument, because the type of
  -- 'x' is already properly sized.
  arr <- lookupLV lv
  lor <- evalE lo
  -- TODO: assert the indices for update are all in bounds
  arr' <- case (arr, lor, x) of
             (BitsV xs, IntV lo', BitsV xs') -> do
                return (BitsV $ arrbulkupd xs lo' xs')
             (ArrayV xs, IntV lo', ArrayV xs') -> do
                return (ArrayV $ arrbulkupd xs lo' xs')
  updateLV lv arr'

            
    
-- Update the ith element of an array
-- Does nothing if the index is out of bounds.
--
-- Note: This is structured to be lazy in the Int argument, because we
-- expect the index to be symbolic, but the list structure to be concrete.
arrupd :: [a] -> Int -> a -> [a]
arrupd [] _ _ = []
arrupd (x:xs) n v = (if (n == 0) then v else x) : arrupd xs (n-1) v

-- List subscript.
-- Assumes the index is in range.
-- Tries to be lazy in the index to work better with symbolic index and
-- concrete list structure.
arrsub :: [a] -> Int -> a
arrsub [x] _ = x
arrsub (x:xs) n = if (n == 0)
                    then x
                    else arrsub xs (n-1)
                    

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

