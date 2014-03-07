
module Static (
    static,
  ) where

import Smten.Prelude

import Smten.Control.Monad.Reader
import Smten.Control.Monad.State
import qualified Smten.Data.Map as Map
import Smten.Data.Functor

import Eval
import Sketch

-- Perform static analysis and evaluation on the given program.
-- Currently does the following: 
--   * evaluate all types.
--      In particular, the width of array types will be of
--      the form: ValE (IntV n)
--   * typechecks the program.
--      Reports an error if the program does not type check.
static :: ProgEnv -> Prog
static p = do
  let readed = runReaderT (mapM staticM (declsof p)) (SR p (error "no top level output type"))
  evalState readed (SS Map.empty)

type TypeEnv = Map.Map Name Type

data SR = SR {
    -- | The program environment.
    sr_env :: ProgEnv,

    -- | The target output type.
    sr_oty :: Type
}

data SS = SS {
    -- | The type environment.
    ss_tyenv :: TypeEnv
}

type SM = ReaderT SR (State SS)

withty :: Type -> SM a -> SM a
withty t = local (\r -> r { sr_oty = t })

class Static a where
   staticM :: a -> SM a

instance Static Type where
   staticM t = do
     env <- asks sr_env
     return $ evalT env t

instance Static Decl where
  staticM (VarD ty nm e) = do
    ty' <- staticM ty
    e' <- withty ty' $ staticM e
    return $ VarD ty' nm e'

  staticM (FunD nm val k) = do
      val' <- staticM val
      return $ FunD nm val' k

instance Static Arg where
  staticM (Arg ty nm) = do
    ty' <- staticM ty
    return $ Arg ty' nm

instance Static Function where
  staticM (Function ty args body) = do
     ty' <- staticM ty
     args' <- mapM staticM args
     let tyenv = Map.fromList [(nm, ty) | Arg ty nm <- args']
     tyenvold <- gets ss_tyenv
     modify $ \s -> s { ss_tyenv = tyenv }
     body' <- withty ty' $ staticM body
     modify $ \s -> s { ss_tyenv = tyenvold }
     return $ Function ty' args' body'

instance Static Stmt where
  staticM (ReturnS x) = ReturnS <$> staticM x
  staticM (AssertS x) = AssertS <$> withty BitT (staticM x)
  staticM (ReorderS xs) = ReorderS <$> mapM staticM xs
  staticM (RepeatS n s) = liftM2 RepeatS (withty IntT $ staticM n) (staticM s)
  staticM (WhileS c s) = liftM2 WhileS (withty BitT $ staticM c) (staticM s)
  staticM (ForS init cond incr body) =
    liftM4 ForS (staticM init) (withty BitT (staticM cond)) (staticM incr) (staticM body)
  staticM (DeclS ty nm) = do
    ty' <- staticM ty
    env <- gets ss_tyenv
    let env' = Map.insert nm ty' env
    modify $ \s -> s { ss_tyenv = env' }
    return $ DeclS ty' nm

  staticM (UpdateS nm e) = do
    env <- asks sr_env
    tyenv <- gets ss_tyenv
    case Map.lookup nm tyenv of
      Nothing -> error $ "variable " ++ nm ++ " not in scope"
      Just ty -> do
        ty' <- staticM ty
        UpdateS nm <$> (withty ty' (staticM e))

  staticM (ArrUpdateS nm idx e) = do
    tyenv <- gets ss_tyenv
    t <- case Map.lookup nm tyenv of
                Just (ArrT t _) -> staticM t
                Just _ -> error $ "variable " ++ nm ++ " is not an array"
                Nothing -> error $ "variable " ++ nm ++ " not in scope"
    idx' <- withty IntT $ staticM idx
    e' <- withty t $ staticM e
    return (ArrUpdateS nm idx' e')

  staticM (ArrBulkUpdateS nm lo w e) = do
    tyenv <- gets ss_tyenv
    t <- case Map.lookup nm tyenv of
                Just t -> staticM t
                Nothing -> error $ "variable " ++ nm ++ " not in scope"
    lo' <- withty IntT $ staticM lo
    w' <- withty IntT $ staticM w
    case (w', t) of
        (ValE (IntV wv), ArrT et (ValE (IntV w)))
           | wv <= w -> do
                e' <- withty (ArrT et (ValE (IntV wv))) $ staticM e
                return (ArrBulkUpdateS nm lo' w' e')
           | otherwise -> error $ "invalid bounds for bulk update of width: " ++ show wv
        (_, ArrT {}) -> error $ "bulk update width could not be determined statically: " ++ show (w')
        (_, _) -> error $ "variable " ++ show nm ++ " is not an array"

  staticM (IfS p a b) = liftM3 IfS (withty BitT $ staticM p) (staticM a) (staticM b)
  staticM (BlockS xs) = blockS <$> mapM staticM xs

instance Static Expr where
  staticM e = do
    dst <- asks sr_oty
    src <- typeof e
    case () of
     _ | src `matches` dst -> staticE e
       | src `subtype` dst -> ICastE dst <$> (withty src $ staticE e)
       | otherwise -> error $ "[000] expected type " ++ show dst ++ " but found type " ++ show src ++ " in the expression " ++ show e

staticE :: Expr -> SM Expr
staticE (AndE a b) = bitwiseop AndE "&" a b
staticE (LAndE a b) = bitwiseop LAndE "&&" a b

staticE (AddE a b) = do
  ty <- asks sr_oty
  case ty of
    IntT -> return ()
    ArrT BitT _ -> return ()
    _ -> error $ "unsupported type for addition: " ++ show ty
  liftM2 AddE (staticM a) (staticM b)

staticE (SubE a b) = intop SubE (-) "-" a b
staticE (LtE a b) = cmpop LtE "<" a b
staticE (GtE a b) = cmpop GtE ">" a b
staticE (LeE a b) = cmpop LeE "<=" a b
staticE (GeE a b) = cmpop GeE ">=" a b
staticE (EqE a b) = eqop EqE "==" a b
staticE (NeqE a b) = eqop NeqE "!=" a b
staticE (CondE p a b) = do
  ty <- asks sr_oty  
  p' <- withty BitT $ staticM p
  a' <- withty ty $ staticM a
  b' <- withty ty $ staticM b
  return $ CondE p' a' b'
staticE (ArrayE a) = do
  ty <- asks sr_oty
  case ty of
      ArrT t _ -> ArrayE <$> withty t (mapM staticM a)
      _ -> error $ "expected array type for array expression, but got: " ++ show ty

staticE (XorE a b) = bitwiseop XorE "xor" a b
staticE (MulE a b) = intop MulE (*) "*" a b
staticE (ModE a b) = intop ModE rem "%" a b
staticE (DivE a b) = intop DivE quot "/" a b
staticE (OrE a b) = bitwiseop OrE "|" a b
staticE (LOrE a b) = bitwiseop LOrE "||" a b
staticE (ShlE a b) = shiftop ShlE "<<" a b
staticE (ShrE a b) = shiftop ShrE ">>" a b
staticE (BitChooseE _ a b) = do
  ty <- asks sr_oty
  bitwiseop (BitChooseE ty) "{|}" a b

staticE (NotE a) = do
  ty <- asks sr_oty
  case ty of
    BitT -> return ()
    ArrT BitT _ -> return ()
    _ -> error $ "unsupported type for not operator: " ++ show ty
  NotE <$> staticM a

staticE (HoleE _ mbnd) = do
  oty <- asks sr_oty
  return $ HoleE oty mbnd
staticE (ValE v) = ValE <$> staticM v
staticE x@(VarE nm) = do
  oty <- asks sr_oty
  ty <- typeof x
  if (ty == oty)
    then return ()
    else error $ "expected type " ++ show oty ++ " but " ++ show nm ++ " has type " ++ show ty

  -- See if we know the value statically.
  -- TODO: This is a hack. We should have a better way of performing static
  -- evaluation.
  env <- asks sr_env
  tyenv <- gets ss_tyenv
  case Map.lookup nm tyenv of
      Just {} -> return x
      Nothing -> case Map.lookup nm env of    
                    Just (VarD _ _ e) -> staticM e
                    _ -> return x

staticE (AccessE a b) = do
  oty <- asks sr_oty
  -- We don't know the width of the array, so we must try to infer it.
  tarr <- typeof a
  case tarr of
    ArrT ty _
      | ty == oty -> liftM2 AccessE (withty tarr $ staticM a) (withty IntT $ staticM b)
      | otherwise -> error $ "[001]expected type " ++ show oty
                             ++ " but found type: " ++ show ty
    _ -> error $ "expected array type, but found type: " ++ show tarr

staticE (BulkAccessE x lo w) = do
  oty <- asks sr_oty
  -- We don't know the width of the array x, so we must try to infer it.
  tarr <- typeof x
  w' <- withty IntT $ staticM w
  case (oty, tarr, w') of
    (ArrT ta (ValE (IntV wdst)),
     ArrT tb (ValE (IntV wsrc)),
     ValE (IntV wv))
      | ta == tb && wdst <= wsrc && wdst == wv -> do
           x' <- withty tarr $ staticM x
           lo' <- withty IntT $ staticM lo
           return $ BulkAccessE x' lo' w'
      | ta == tb -> error $ "bulk array access out of bounds"
      | otherwise -> error $ "[002]expected type " ++ show oty
                             ++ " but found type: " ++ show tarr
    (_, ArrT {}, ValE (IntV {})) -> error $ "could not determine array width statically: " ++ show tarr
    (_, ArrT {}, _) -> error $ "could not determine bulk width statically: " ++ show w'
    _ -> error $ "expected array type, but found type: " ++ show tarr

staticE (CastE t e) = do
  oty <- asks sr_oty
  t' <- staticM t
  if t' == oty
    then return ()
    else error $ "[003]expected type " ++ show oty ++ " but found type: " ++ show t
  te <- typeof e
  case te of
    UnknownT -> error $ "unable to determine type of cast argument"
    _ -> CastE t' <$> (withty te $ staticM e)

-- Note: I don't expect this case to happen, because implicit casts are not
-- introduced before this phase.
staticE (ICastE {}) = error "implicit cast in static phase"
  
staticE (AppE fnm xs) = do
  oty <- asks sr_oty
  env <- asks sr_env
  case Map.lookup fnm env of
     Just (FunD _ f _) -> do
        FunT foty txs <- staticM $ f_type f
        if oty == foty
           then return ()
           else error $ "[004]expected type " ++ show oty ++ " but found type: " ++ show foty

        if length txs == length xs
           then return ()
           else error $ "wrong number of arguments passed to function " ++ fnm

        xs' <- sequence [withty t (staticM x) | (t, x) <- zip txs xs]
        return $ AppE fnm xs'
     Nothing -> error $ "function " ++ show fnm ++ " not found"

instance Static Value where
  staticM x@(IntV v) = do
    ty <- asks sr_oty
    case ty of
      IntT -> return x
      BitT -> case v of
                0 -> return $ BitV False
                1 -> return $ BitV True
                _ -> error $ "cannot use integer literal as a bit: " ++ show v
      ArrT _ (ValE (IntV 0)) -> error $ "TODO: integer literal for 0 length array"
      ArrT BitT (ValE (IntV w)) ->
         case v of
           0 -> staticM (arrayV $ BitV False : replicate (w-1) (pad BitT))
           1 -> staticM (arrayV $ BitV True : replicate (w-1) (pad BitT))
           _ -> error $ "cannot use integer literal for bits: " ++ show v
      ArrT t (ValE (IntV w)) -> staticM (arrayV $ x : replicate (w-1) (pad t))
      _ -> error $ "unsupported type for integer literal: " ++ show ty

  staticM x@(BitV _) = do
    ty <- asks sr_oty
    case ty of
      BitT -> return x
      _ -> error $ "bit type found where expected type was: " ++ show ty

  staticM x@(BitsV _) = do
    ty <- asks sr_oty
    case ty of
      ArrT BitT _ -> return x
      _ -> error $ "expected type " ++ show ty ++ " but got type bits"

  staticM x@(ArrayV _) = do
    ty <- asks sr_oty
    case ty of
      ArrT {} -> return x
      _ -> error $ "expected type " ++ show ty ++ " but got array type"
      

  -- I don't expect to find any other value in the program at this point.
  staticM v = error $ "TODO: Static.staticM for value: " ++ show v

-- Try to unify the given types A and B.
-- Returns a type C which is consistent with both argument types where
-- possible:  A is a subtype of C
--            B is a subtype of C
unify :: Type -> Type -> Type
unify a b
  | a == b = a
unify UnknownT b = b
unify a UnknownT = a
unify IntT BitT = IntT
unify BitT IntT = IntT
unify (ArrT a (ValE (IntV wa))) (ArrT b (ValE (IntV wb)))
  | a == b = ArrT a (ValE (IntV (max wa wb)))
unify a b
  | dimension a < dimension b = unify (ArrT a (ValE (IntV 1))) b
  | dimension a > dimension b = unify a (ArrT b (ValE (IntV 1)))
unify a b = error $ "unable to unify types: " ++ show (a, b)

-- Given types A and B,
--  return True if B = A after substition of UnknownT types in A.
matches :: Type -> Type -> Bool
matches a b | a == b = True
matches UnknownT _ = True
matches (FunT a as) (FunT b bs) = and (zipWith matches (a:as) (b:bs))
matches (ArrT a (ValE wa)) (ArrT b (ValE wb)) = wa == wb && a `matches` b
matches _ _ = False

-- Comparison operators: <, >, <=, >=
-- Typing Rules:
--   * Return type is bit
--   * Argument types are int.
cmpop :: (Expr -> Expr -> Expr) -> String -> Expr -> Expr -> SM Expr
cmpop f nm a b = do
    ty <- asks sr_oty
    case ty of
      BitT -> return ()
      _ -> error $ "unsupported return type for " ++ nm ++ " operator: " ++ show ty
    liftM2 f (withty IntT $ staticM a) (withty IntT $ staticM b)


-- Equality operators: == and !=
-- Typing Rules:
--   * Return type is bit
--   * Argument types are the same.
eqop :: (Expr -> Expr -> Expr) -> String -> Expr -> Expr -> SM Expr
eqop f nm a b = do
    ty <- asks sr_oty
    case ty of
      BitT -> return ()
      _ -> error $ "unsupported return type for " ++ nm ++ " operator: " ++ show ty
    -- We need to determine the argument types here.
    -- See if we can infer them from the arguments.
    targ <- liftM2 unify (typeof a) (typeof b)
    case targ of
      UnknownT -> error $ "ambiguous type for " ++ nm ++ " operator"
      BitT -> return ()
      IntT -> return ()
      ArrT BitT _ -> return ()
      _ -> error $ "TODO: does " ++ nm ++ " operator support type: " ++ show targ
    liftM2 f (withty targ $ staticM a) (withty targ $ staticM b)

-- Bitwise Operators
-- Typing Rules:
--   * The return type is either bit or bit[]
--   * The argument types match the return type.
bitwiseop :: (Expr -> Expr -> Expr) -> String -> Expr -> Expr -> SM Expr
bitwiseop f nm a b = do
    ty <- asks sr_oty
    case ty of
      BitT -> return ()
      ArrT BitT _ -> return ()
      _ -> error $ "unsupported type for bitwise " ++ nm ++ ": " ++ show ty
    liftM2 f (staticM a) (staticM b)

-- Integer operators
-- Typing Rules:
--   * The return type is int
--   * The argument types are int
intop :: (Expr -> Expr -> Expr)
         -> (Int -> Int -> Int)
         -> String -> Expr -> Expr -> SM Expr
intop mk f nm a b = do
    ty <- asks sr_oty
    case ty of
      IntT -> return ()
      _ -> error $ "unsupported type for operator " ++ nm ++ ": " ++ show ty
    a' <- staticM a
    b' <- staticM b
    case (a', b') of
       (ValE (IntV av), ValE (IntV bv)) -> return $ ValE (IntV (f av bv))
       _ -> return $ mk a' b'

-- Shift operators
-- Typing Rules:
--   * The return type is bit[] or int.
--   * The first argument type matches the return type
--   * The second argument type is Int.
shiftop :: (Expr -> Expr -> Expr) -> String -> Expr -> Expr -> SM Expr
shiftop f nm a b = do
    ty <- asks sr_oty
    ty' <- case ty of
              ArrT BitT _ -> return ty
              IntT -> return ty
              _ -> error $ "unsupported type for operator " ++ nm ++ ": " ++ show ty
    liftM2 f (withty ty $ staticM a) (withty IntT $ staticM b)

-- return the type of a shift expression given its first argument.
typeofshift :: Expr -> SM Type
typeofshift a = do
  ty <- typeof a
  return $ case ty of
             ArrT BitT _ -> ty
             IntT -> ty
             _ -> UnknownT

-- Determine as best as possible the type of the given expression.
-- Returns 'UnknownT' if the type is not certain.
typeof :: Expr -> SM Type
typeof (ValE v) = return $ typeofV v
typeof (AndE a b) = liftM2 unify (typeof a) (typeof b)
typeof (LAndE a b) = liftM2 unify (typeof a) (typeof b)
typeof (AddE a b) = liftM2 unify (typeof a) (typeof b)
typeof (SubE a b) = return IntT
typeof (LtE a b) = return BitT
typeof (GtE a b) = return BitT
typeof (LeE a b) = return BitT
typeof (GeE a b) = return BitT
typeof (EqE a b) = return BitT
typeof (NeqE a b) = return BitT
typeof (ArrayE a) = do
   ty <- foldr unify UnknownT <$> mapM typeof a
   return $ ArrT ty (ValE (IntV (length a)))
typeof (XorE a b) = liftM2 unify (typeof a) (typeof b)
typeof (MulE a b) = return IntT
typeof (ModE a b) = return IntT
typeof (DivE a b) = return IntT
typeof (CondE _ a b) = liftM2 unify (typeof a) (typeof b)
typeof (OrE a b) = liftM2 unify (typeof a) (typeof b)
typeof (LOrE a b) = liftM2 unify (typeof a) (typeof b)
typeof (ShlE a b) = typeofshift a
typeof (ShrE a b) = typeofshift a
typeof (NotE a) = typeof a
typeof (HoleE ty _) = return ty
typeof (BitChooseE _ a b) = liftM2 unify (typeof a) (typeof b)
typeof (VarE nm) = do
    env <- asks sr_env
    tyenv <- gets ss_tyenv
    case Map.lookup nm tyenv of
        Just v -> staticM v
        Nothing -> case Map.lookup nm env of    
                      Just d -> staticM $ d_type d
                      Nothing -> return UnknownT
typeof (AccessE a b) = do
    ta <- typeof a
    case ta of
        ArrT t _ -> return t
        _ -> return UnknownT

typeof (BulkAccessE a b c) = do
    ta <- typeof a
    case (ta, c) of
        (ArrT t _, ValE (IntV w)) -> return $ ArrT t (ValE (IntV w))
        _ -> return UnknownT
typeof (CastE t e) = staticM t
typeof (AppE nm xs) = do
    env <- asks sr_env
    case Map.lookup nm env of
       Just (FunD _ (Function v _ _) _) -> staticM v
       Nothing -> return UnknownT

-- subtype a b
--   Test whether the type 'a' is a proper subtype of the type 'b'
subtype :: Type -> Type -> Bool
subtype a b
  | a == b = True
  | dimension a < dimension b = subtype (ArrT a (ValE (IntV 1))) b
subtype BitT IntT = True
subtype (ArrT ta (ValE (IntV wa))) (ArrT tb (ValE (IntV wb)))
  | ta `subtype` tb && wa <= wb = True
subtype _ _ = False

