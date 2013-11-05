
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
  evalState readed (SS (error "TODO: static with global tyenv"))

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

instance Static Function where
  staticM (Function ty args body) = do
     ty' <- staticM ty
     let FunT oty argtys = ty'
         tyenv = Map.fromList (zip args argtys)
     tyenvold <- gets ss_tyenv
     modify $ \s -> s { ss_tyenv = tyenv }
     body' <- withty oty $ staticM body
     modify $ \s -> s { ss_tyenv = tyenvold }
     return $ Function ty' args body'

instance Static Stmt where
  staticM (ReturnS x) = ReturnS <$> staticM x
  staticM (AssertS x) = AssertS <$> withty BitT (staticM x)
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
    env <- gets ss_tyenv
    case Map.lookup nm env of
      Nothing -> error $ "variable " ++ nm ++ " not in scope"
      Just ty -> UpdateS nm <$> (withty ty (staticM e))

  staticM (ArrUpdateS nm idx e) = do
    env <- gets ss_tyenv
    let t = case Map.lookup nm env of
                Just (ArrT t _) -> t
                Just _ -> error $ "variable " ++ nm ++ " is not an array"
                Nothing -> error $ "variable " ++ nm ++ " not in scope"
    idx' <- withty IntT $ staticM idx
    e' <- withty t $ staticM e
    return (ArrUpdateS nm idx' e')

  staticM (IfS p a b) = liftM3 IfS (withty BitT $ staticM p) (staticM a) (staticM b)
  staticM (BlockS xs) = blockS <$> mapM staticM xs

instance Static Expr where
  staticM (AndE a b) = bitwiseop AndE "and" a b

  staticM (AddE a b) = do
    ty <- asks sr_oty
    case ty of
      IntT -> return ()
      ArrT BitT _ -> return ()
      _ -> error $ "unsupported type for addition: " ++ show ty
    liftM2 AddE (staticM a) (staticM b)

  staticM (SubE a b) = intop SubE "-" a b
  staticM (LtE a b) = cmpop LtE "<" a b
  staticM (GtE a b) = cmpop GtE ">" a b
  staticM (LeE a b) = cmpop LeE "<=" a b
  staticM (GeE a b) = cmpop GeE ">=" a b
  staticM (EqE a b) = eqop EqE "==" a b
  staticM (NeqE a b) = eqop NeqE "!=" a b
  staticM (ArrayE a) = do
    ty <- asks sr_oty
    case ty of
        ArrT t _ -> ArrayE <$> withty t (mapM staticM a)
        _ -> error $ "expected array type for array expression, but got: " ++ show ty

  staticM (XorE a b) = bitwiseop XorE "xor" a b
  staticM (MulE a b) = intop MulE "*" a b
  staticM (ModE a b) = intop ModE "%" a b
  staticM (OrE a b) = bitwiseop OrE "or" a b
  staticM (LOrE a b) = bitwiseop LOrE "||" a b
  staticM (LAndE a b) = bitwiseop LAndE "&&" a b
  staticM (ShlE a b) = shiftop ShlE "<<" a b
  staticM (ShrE a b) = shiftop ShrE ">>" a b
  staticM (BitChooseE a b) = bitwiseop BitChooseE "{|}" a b

  staticM (NotE a) = do
    ty <- asks sr_oty
    case ty of
      BitT -> return ()
      ArrT BitT _ -> return ()
      _ -> error $ "unsupported type for not operator: " ++ show ty
    NotE <$> staticM a

  staticM x@(HoleE bnd) = return x
  staticM (ValE v) = ValE <$> staticM v
  staticM x@(VarE nm) = do
    oty <- asks sr_oty
    ty <- typeof x
    if (ty `subtype` oty)
      then return x
      else error $ "expected type " ++ show oty ++ " but " ++ show nm ++ " has type " ++ show ty

  staticM (AccessE a b) = do
    oty <- asks sr_oty
    -- We don't know the width of the array, so we must try to infer it.
    tarr <- typeof a
    case tarr of
      ArrT ty _
        | ty == oty -> liftM2 AccessE (withty tarr $ staticM a) (withty IntT $ staticM b)
        | otherwise -> error $ "expected type " ++ show oty
                               ++ " but found type: " ++ show ty
      _ -> error $ "expected array type, but found type: " ++ show tarr

  staticM (CastE t e) = do
    oty <- asks sr_oty
    t' <- staticM t
    if t' == oty
      then return ()
      else error $ "expected type " ++ show oty ++ " but found type: " ++ show t
    te <- typeof e
    case te of
      UnknownT -> error $ "unable to determine type of cast argument"
      _ -> CastE t' <$> (withty te $ staticM e)

  staticM (AppE fnm xs) = do
    oty <- asks sr_oty
    env <- asks sr_env
    case Map.lookup fnm env of
       Just (FunD _ f _) -> do
          let FunT foty txs = evalT env $ f_type f
          if oty == foty
             then return ()
             else error $ "expected type " ++ show oty ++ " but found type: " ++ show foty

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
      ArrT BitT _ ->
              case v of
                0 -> staticM (arrayV [BitV False])
                1 -> staticM (arrayV [BitV True])
                _ -> error $ "cannot use integer literal for bits: " ++ show v
      ArrT t _ -> staticM (arrayV [x])
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

-- Try to unify the given types.
unify :: Type -> Type -> Type
unify a b | a == b = a
unify UnknownT b = b
unify a UnknownT = a
unify a b = error $ "unable to unify types: " ++ show (a, b)


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
intop :: (Expr -> Expr -> Expr) -> String -> Expr -> Expr -> SM Expr
intop f nm a b = do
    ty <- asks sr_oty
    case ty of
      IntT -> return ()
      _ -> error $ "unsupported type for operator " ++ nm ++ ": " ++ show ty
    liftM2 f (staticM a) (staticM b)

-- Shift operators
-- Typing Rules:
--   * The return type is bit[]
--   * The first argument type matches the return type
--   * The second argument type is Int.
shiftop :: (Expr -> Expr -> Expr) -> String -> Expr -> Expr -> SM Expr
shiftop f nm a b = do
    ty <- asks sr_oty
    case ty of
      ArrT BitT _ -> return ()
      _ -> error $ "unsupported type for operator " ++ nm ++ ": " ++ show ty
    liftM2 f (withty ty $ staticM a) (withty IntT $ staticM b)

-- Determine as best as possible the type of the given expression.
-- Returns 'UnknownT' if the type is not certain.
typeof :: Expr -> SM Type
typeof (ValE v) = return $ typeofV v
typeof (AndE a b) = liftM2 unify (typeof a) (typeof b)
typeof (AddE a b) = liftM2 unify (typeof a) (typeof b)
typeof (SubE a b) = return IntT
typeof (LtE a b) = return BitT
typeof (GtE a b) = return BitT
typeof (LeE a b) = return BitT
typeof (GeE a b) = return BitT
typeof (EqE a b) = return BitT
typeof (NeqE a b) = return BitT
typeof (ArrayE a) = return UnknownT
typeof (XorE a b) = liftM2 unify (typeof a) (typeof b)
typeof (MulE a b) = return IntT
typeof (ModE a b) = return IntT
typeof (OrE a b) = liftM2 unify (typeof a) (typeof b)
typeof (LOrE a b) = liftM2 unify (typeof a) (typeof b)
typeof (LAndE a b) = liftM2 unify (typeof a) (typeof b)
typeof (ShlE a b) = typeof a
typeof (ShrE a b) = typeof a
typeof (NotE a) = typeof a
typeof (HoleE bnd) = return UnknownT
typeof (BitChooseE a b) = liftM2 unify (typeof a) (typeof b)
typeof (VarE nm) = do
    env <- asks sr_env
    tyenv <- gets ss_tyenv
    case Map.lookup nm tyenv of
        Just v -> return v
        Nothing -> case Map.lookup nm env of    
                      Just d -> return (d_type d)
                      Nothing -> return UnknownT
typeof (AccessE a b) = do
    ta <- typeof a
    case ta of
        ArrT t _ -> return t
        _ -> return UnknownT
typeof (CastE t e) = return t
typeof (AppE nm xs) = do
    env <- asks sr_env
    case Map.lookup nm env of
       Just (FunD _ (Function (FunT v _) _ _) _) -> return v
       Nothing -> return UnknownT

-- subtype a b
--   Test whether the type 'a' is a proper subtype of the type 'b'
subtype :: Type -> Type -> Bool
subtype a b | a == b = True
subtype BitT IntT = True
subtype _ _ = False

