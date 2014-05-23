
module Eval (
    evalP, evalT_xxx,
    StmtResult(..), evalS, evalE, -- Exposed only for performance debugging
  ) where

import Smten.Prelude
import Smten.Control.Monad
import Smten.Data.Functor
import Smten.Data.List
import Smten.Data.Maybe
import qualified Smten.Data.Map as Map

import Bits
import EvalMonad
import Input
import IntS
import Program
import Syntax

-- Evaluate a sketch program on the given program input.
--   It runs each harness on the harness's input, and returns whether all the
--   harnesses completed successfully.
evalP :: Program -> ProgramInput -> Bool
evalP p i = all (evalD p i) (decls p)

-- Evaluate a type.
-- It should not fail.
-- TODO: Remove this. We shouldn't have to evaluate types statically.
evalT_xxx :: Program -> Type -> Type
evalT_xxx env (ArrT t e) = ArrT (evalT_xxx env t) $ ValE (fromMaybe (error "evalT_xxx failed") $ runEvalM env (evalE e))
evalT_xxx env (FunT x xs) = FunT (evalT_xxx env x) (map (evalT_xxx env) xs)
evalT_xxx env t = t

evalD :: Program -> ProgramInput -> Decl -> Bool
evalD env i (VarD {}) = True
evalD env i d@(FunD {}) =
  case fd_kind d of
    NormalF -> True
    GeneratorF -> True
    HarnessF
      | Just args <- map ValE <$> Map.lookup (declN d) i ->
          isJust (runEvalM env (apply' (fd_val d) args))
    WithSpecF snm
      | Just sd@(FunD {}) <- Map.lookup snm env
      , Just inputs <- Map.lookup (declN d) i ->
          let ivars = ["__i" ++ show n | n <- [1..length inputs]]
              args = map VarE ivars

              want = runEvalM env $ do
                zipWithM declVar ivars inputs
                apply' (fd_val sd) args

              got = runEvalM env $ do
                zipWithM declVar ivars inputs
                apply' (fd_val d) args
          in want == got
evalD env i (StructD {}) = True

-- Apply a function to the given arguments.
-- Returns the resulting value, and any updated values for reference
-- arguments.
apply' :: Function -> [Expr] -> EvalM (Value, [Maybe Value])
apply' f xs = do
    xs' <- mapM evalE xs
    let argnms = [nm | Arg nm _ _ <- f_args f]

        getref :: Arg -> EvalM (Maybe Value)
        getref (Arg _ _ False) = return Nothing
        getref (Arg nm _ True) = lookupVar nm

    scoped $ do
       zipWithM declVar argnms xs'
       res <- returned <$> evalS (f_body f)
       refs <- mapM getref (f_args f)
       return (res, refs)

-- Apply a function to the given arguments.
-- Returns the resulting value, and commits updates to reference arguments to
-- the local scope.
-- Any arguments passed by reference should be value LVals.
apply :: Function -> [Expr] -> EvalM Value
apply f xs = do
    let updref :: Expr -> Maybe Value -> EvalM ()
        updref _ Nothing = return ()
        updref x (Just v) = do
           let lv = fromMaybe (error "updref arg not an lval") $ asLVal x
           updateLV lv v
    (res, refs) <- apply' f xs
    zipWithM_ updref xs refs
    return res

data StmtResult = OK | RET Value
  deriving (Eq)

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
  let iter = do
        c' <- evalE cond
        case c' of
          BitV False -> return OK
          BitV True -> do
            ret <- evalS body
            case ret of
              OK -> evalS incr >> iter
              RET v -> return (RET v)
  iter
evalS w@(WhileS c s) = {-# SCC "WhileS" #-} do
  c' <- {-# SCC "WhileSCondition" #-} evalE c
  case c' of
    BitV False -> return OK
    BitV True -> do
      ret <- evalS s
      case ret of
        OK -> evalS w
        RET v -> return (RET v)
evalS (DeclS ty vars) = {-# SCC "DeclS" #-} do
  let f (nm, mval) = do
          let defv = case ty of
                        t@(ArrT {}) -> pad t
                        IntT -> IntV 0  -- TODO: not sure if this should be 0 or uninitialized
                        _ -> error $ "variable " ++ nm ++ " uninitialized"
          v0 <- evalE $ fromMaybe (ValE defv) mval
          declVar nm v0
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
evalE (BinaryE (ChoiceOp {}) _ _) = error "ChoiceOp encountered in evalE"
evalE (BinaryE AndOp a b) = {-# SCC "AndE" #-} do
    a' <- evalE a
    b' <- evalE b
    case (a', b') of
      (BitsV av, BitsV bv) -> return $ BitsV (av `andB` bv)
      (BitV av, BitV bv) -> return $ BitV (av && bv)
evalE (BinaryE LAndOp a b) = {-# SCC "LAndE" #-} do
    a' <- evalE a
    case a' of
        BitV True -> evalE b
        BitV False -> return a'
        BitsV av -> do  -- TODO: should we short circuit if av is 0?
            b' <- evalE b
            case b' of
                BitsV bv -> return $ BitsV (av `andB` bv)
evalE (BinaryE OrOp a b) = {-# SCC "OrE" #-} do
    a' <- evalE a
    b' <- evalE b
    case (a', b') of
      (BitsV av, BitsV bv) -> return $ BitsV (av `orB` bv)
      (BitV av, BitV bv) -> return $ BitV (av || bv)
evalE (BinaryE LOrOp a b) = {-# SCC "LOrE" #-} do
    a' <- evalE a
    case a' of
        BitV True -> return a'
        BitV False -> evalE b
        BitsV av -> do  -- TODO: should we short circuit if av is all ones?
            b' <- evalE b
            case b' of
                BitsV bv -> return $ BitsV (av `orB` bv)
evalE (BinaryE AddOp a b) = {-# SCC "AddE" #-} liftM2 (+) (evalE a) (evalE b)
evalE (BinaryE SubOp a b) = {-# SCC "SubE" #-} liftM2 (-) (evalE a) (evalE b)
evalE (BinaryE LtOp a b) = BitV <$> liftM2 (<) (evalE a) (evalE b)
evalE (BinaryE GtOp a b) = BitV <$> liftM2 (>) (evalE a) (evalE b)
evalE (BinaryE LeOp a b) = BitV <$> liftM2 (<=) (evalE a) (evalE b)
evalE (BinaryE GeOp a b) = BitV <$> liftM2 (>=) (evalE a) (evalE b)
evalE (BinaryE EqOp a b) = BitV <$> liftM2 (==) (evalE a) (evalE b)
evalE (BinaryE NeqOp a b) = BitV <$> liftM2 (/=) (evalE a) (evalE b)
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
evalE (BinaryE XorOp a b) = {-# SCC "XorE" #-} do
    a' <- evalE a
    b' <- evalE b
    case (a', b') of
      (BitsV av, BitsV bv) -> return $ BitsV (av `xorB` bv)
      (BitV av, BitV bv) -> return $ BitV (av `xor` bv)
evalE (BinaryE MulOp a b) = liftM2 (*) (evalE a) (evalE b)
evalE (BinaryE ModOp a b) = {-# SCC "ModE" #-} do
    a' <- evalE a
    b' <- evalE b
    case (a', b') of
      (IntV av, IntV bv) -> do
         assert (bv /= 0)
         return $ IntV (av `rem` bv)
evalE (BinaryE DivOp a b) = {-# SCC "DivE" #-} do
    a' <- evalE a
    b' <- evalE b
    case (a', b') of
      (IntV av, IntV bv) -> do
         assert (bv /= 0)
         return $ IntV (av `quot` bv)
evalE (BinaryE ShlOp a b) = {-# SCC "ShlE" #-} do
    a' <- evalE a
    b' <- evalE b
    case (a', b') of
      (BitsV av, IntV bv) -> return $ BitsV (av `shlB` bv)
      (IntV av, IntV bv) -> return $ IntV (av `shlI` bv)
evalE (BinaryE ShrOp a b) = {-# SCC "ShrE" #-} do
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
evalE (HoleE {}) = error "HoleE in evalE"
evalE (ChoiceE {}) = error "ChoiceE in evalE"
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
evalE (FieldE a m) = do
    a' <- evalE a
    fieldAccess a' m

evalE (NewE nm init) = do
  fields <- lookupStructType nm
  let mkInit :: (Name, Type) -> EvalM (Name, Value)
      mkInit (n, ty)
        | Just e <- lookup n init = do
            v <- evalE e
            return (n, v)
        | otherwise = return (n, pad ty)
  initialized <- mapM mkInit fields
  PointerV <$> newStruct (Map.fromList initialized)

evalE (CastE t e) = {-# SCC "CastE" #-} do
    e' <- evalE e
    case (t, e') of
        (IntT, BitV v) -> return (IntV (if v then 1 else 0))
        (IntT, BitsV v) -> return (intV (valB v))
        (ArrT BitT w, BitV x) -> do
            IntV w' <- evalE w
            return (BitsV (x : genericReplicate (w'-1) False))
        (ArrT BitT w, BitsV xs) -> do
            IntV w' <- evalE w
            return (BitsV (genericTake w' (xs ++ genericReplicate w' False)))
        (ArrT t w, ArrayV xs) -> do
            IntV w' <- evalE w
            return (ArrayV (genericTake w' (xs ++ genericReplicate w' (pad t))))
        _ -> error $ "Unsupported cast of " ++ show e' ++ " to type " ++ show t
evalE (ICastE dst e) = do
    dst' <- evalT dst
    icast dst' <$> evalE e
evalE (AppE f xs) = {-# SCC "AppE" #-} do
    f' <- evalE (VarE f)
    case f' of
        FunV fv -> apply fv xs
        _ -> error $ "Expected function, but got: " ++ show f'

evalT :: Type -> EvalM Type
evalT (ArrT t w) = do
    t' <- evalT t
    w' <- evalE w
    return (ArrT t' (ValE w'))
evalT (FunT x xs) = do
    x' <- evalT x
    xs' <- mapM evalT xs
    return (FunT x' xs')
evalT t = return t

-- Given an array value and index, return the value at that index.
-- TODO: make an LValE, have it use lookupLV, then inline this into lookupLV.
arrayAccess :: Value -> Expr -> EvalM Value
arrayAccess arr i = do
    i' <- evalE i
    let idx = case i' of
               IntV iv -> iv
    case arr of
        BitsV av -> do
          assert (idx >= 0 && idx < genericLength av)
          return (BitV (arrsub av idx))
        ArrayV xs -> do
          assert (idx >= 0 && idx < genericLength xs)
          return (arrsub xs idx)

bulkAccess :: Value -> Expr -> Expr -> EvalM Value
bulkAccess arr lo w = do
    IntV lo' <- evalE lo
    IntV w' <- evalE w
    case arr of
        BitsV xs -> do
          let xs' = genericDrop lo' xs
          assert (lo' >= 0 && w' <= fromInt (length xs'))
          return (BitsV (genericTake w' xs'))
        ArrayV xs -> do
          let xs' = genericDrop lo' xs
          assert (lo' >= 0 && w' <= fromInt (length xs'))
          return (ArrayV (genericTake w' xs'))

-- Access a field of a structure
fieldAccess :: Value -> Name -> EvalM Value
fieldAccess x m = do
    case x of
      PointerV ptr -> do
        fields <- lookupStruct ptr
        return . fromMaybe (error "fieldAccess: no such member") $ Map.lookup m fields
    
lookupLV :: LVal -> EvalM Value
lookupLV (VarLV nm) = fromMaybe (error "lookupLV: no such var") <$> lookupVar nm
lookupLV (ArrLV lv i) = do
    arr <- lookupLV lv
    arrayAccess arr i
lookupLV (BulkLV lv lo w) = do
    arr <- lookupLV lv
    bulkAccess arr lo w
lookupLV (FieldLV lv m) = do
    x <- lookupLV lv
    fieldAccess x m

updateLV :: LVal -> Value -> EvalM ()
updateLV (VarLV nm) x = insertVar nm x
updateLV (ArrLV lv i) x = do
  arr <- lookupLV lv
  ir <- evalE i
  arr' <- case (arr, ir, x) of
             (BitsV xs, IntV i', BitV e') -> do
                assert (i' < fromInt (length xs))
                return (BitsV $ arrupd xs i' e')
             (ArrayV xs, IntV i', e') -> do
                assert (i' < fromInt (length xs))
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
updateLV (FieldLV lv m) v = do
  PointerV ptr <- lookupLV lv
  fields <- lookupStruct ptr
  updateStruct ptr (Map.insert m v fields)
    
-- Update the ith element of an array
-- Does nothing if the index is out of bounds.
--
-- Note: This is structured to be lazy in the Int argument, because we
-- expect the index to be symbolic, but the list structure to be concrete.
arrupd :: [a] -> IntS -> a -> [a]
arrupd [] _ _ = []
arrupd (x:xs) n v = (if (n == 0) then v else x) : arrupd xs (n-1) v

-- List subscript.
-- Assumes the index is in range.
-- Tries to be lazy in the index to work better with symbolic index and
-- concrete list structure.
arrsub :: [a] -> IntS -> a
arrsub [x] _ = x
arrsub (x:xs) n = if (n == 0)
                    then x
                    else arrsub xs (n-1)
                    

-- Do a bulk update starting at the given index.
arrbulkupd :: [a] -> IntS -> [a] -> [a]
arrbulkupd vals i vals' =
  let lo = genericTake i vals
      mid = vals'
      hi = genericDrop (i + genericLength vals') vals
  in concat [lo, mid, hi]

-- Implicitly cast the given value to the given type.
icast :: Type -> Value -> Value
icast dst v | dst == typeofV v = v
icast IntT (BitV False) = IntV 0
icast IntT (BitV True) = IntV 1
icast IntT v@(IntV {}) = v
icast (StructT {}) v@(PointerV {}) = v
icast (ArrT BitT (ValE (IntV w))) (BitsV xs)
 | genericLength xs <= w = BitsV (genericTake w (xs ++ genericReplicate w False))
 | otherwise = error $ "Implicit cast would truncate a bit vector"
icast t@(ArrT {}) (BitsV xs) = icast t (ArrayV (map BitV xs))
icast (ArrT t (ValE (IntV w))) (ArrayV xs)
 | genericLength xs <= w = ArrayV $ genericTake w (map (icast t) xs ++ genericReplicate w (pad t))
 | otherwise = error $ "Implicit cast would truncate an array"
icast t@(ArrT {}) v = icast t (arrayV [v])
icast t v = error $ "TODO: implement implicit cast of " ++ show v ++ " to " ++ show t

shlI :: (Num int, Ord int) => int -> int -> int
shlI x n
 | n < 0 = 0
 | n == 0 = x
 | otherwise = shlI (2*x) (n-1)

shrI :: (Integral int) => int -> int -> int
shrI x n
 | n < 0 = 0
 | n == 0 = x
 | otherwise = shrI (x `quot` 2) (n-1)

