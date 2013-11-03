
module Generate (generate) where

import Smten.Prelude
import Smten.Control.Monad.Reader
import Smten.Control.Monad.State
import Smten.Data.Functor
import qualified Smten.Data.Map as Map
import Smten.Symbolic

import Bits
import Eval
import Input
import Sketch

-- Given a program with explicit holes, generate a corrisponding symbolic
-- candidate program without holes.
generate :: ProgEnv -> Symbolic Prog
generate p = do
  let readed = runReaderT (mapM genD (declsof p)) (GR p (error "no top level output type"))
  (ds, s) <- runStateT readed (TS (error "TODO: generate with global tyenv") [])
  return $ ts_decls s ++ ds

type TypeEnv = Map.Map Name Type

data GR = GR {
    -- | The program environment.
    gr_env :: ProgEnv,

    -- | The target output type.
    gr_oty :: Type
}

data TS = TS {
    -- | The type environment.
    ts_tyenv :: TypeEnv,

    -- | Emmitted (generated) declarations
    ts_decls :: [Decl]
}

type GM = ReaderT GR (StateT TS Symbolic)

withty :: Type -> GM a -> GM a
withty t = local (\r -> r { gr_oty = t })

-- Emit a declaration. Use a template for the declaration name, and a
-- uniquified version of that name will be used instead and returned. 
emit :: Decl -> GM Name
emit d = do
    s <- get
    let nm = d_name d ++ "_" ++ show (length $ ts_decls s)
    put $ s { ts_decls = d { d_name = nm } : ts_decls s }
    return nm

liftSymbolic :: Symbolic a -> GM a
liftSymbolic = lift . lift

genD :: Decl -> GM Decl
genD d@(VarD {}) = return d
genD d@(FunD {}) = do
    let FunT oty argtys = f_type . fd_val $ d
        tyenv = Map.fromList (zip (f_args . fd_val $ d) argtys)
    tyenvold <- gets ts_tyenv
    modify $ \s -> s { ts_tyenv = tyenv }
    body' <- withty oty $ genS (f_body . fd_val $ d)
    let val' = (fd_val d) { f_body = body' }
    modify $ \s -> s { ts_tyenv = tyenvold }
    return $ d { fd_val = val' }

genS :: Stmt -> GM Stmt
genS (ReturnS x) = ReturnS <$> genE x
genS (AssertS x) = AssertS <$> withty BitT (genE x)
genS (RepeatS en s) = do
  -- TODO: evaluate n statically as much as possible here
  -- Perhaps a 'simplify' operation on expressions would be useful, both here
  -- and for evalT?
  --
  -- TODO: what if the statement changes the condition? How do we keep track
  -- of the original condition?
  let count = case en of
                ValE (IntV v) -> v
                _ -> bnd_unroll_amnt
  en' <- withty IntT $ genE en
  let unroll n
        | n >= count = blockS []
        | otherwise =
            let ifthen = blockS [s, unroll (n+1)]
                ifelse = (blockS [])
            in IfS (GtE en' (ValE (IntV n))) ifthen ifelse
  genS (unroll 0)

genS (WhileS c s) = liftM2 WhileS (withty BitT (genE c)) (genS s)
genS (ForS init cond incr body) =
    liftM4 ForS (genS init) (withty BitT (genE cond)) (genS incr) (genS body)
genS s@(DeclS ty nm) = do
  env <- gets ts_tyenv
  let env' = Map.insert nm ty env
  modify $ \s -> s { ts_tyenv = env' }
  return s
genS (UpdateS nm e) = do
  env <- gets ts_tyenv
  case Map.lookup nm env of
    Nothing -> error $ "variable " ++ nm ++ " not in scope"
    Just ty@(ArrT {}) -> do
        -- insert an explicit cast here so padding is performed as needed.
        e' <- withty ty (genE e)
        return $ UpdateS nm (CastE ty e')
    Just ty -> UpdateS nm <$> (withty ty (genE e))
genS (ArrUpdateS nm idx e) = do
    env <- gets ts_tyenv
    let t = case Map.lookup nm env of
                Just (ArrT t _) -> t
                Just _ -> error $ "variable " ++ nm ++ " is not an array"
                Nothing -> error $ "variable " ++ nm ++ " not in scope"
    idx' <- withty IntT $ genE idx
    e' <- withty t $ genE e
    return (ArrUpdateS nm idx' e')
genS (IfS p a b) = liftM3 IfS (withty BitT $ genE p) (genS a) (genS b)
genS (BlockS xs) = blockS <$> mapM genS xs

genE :: Expr -> GM Expr
genE (AndE a b) = liftM2 AndE (genE a) (genE b)
genE (AddE a b) = liftM2 AddE (genE a) (genE b)
genE (SubE a b) = liftM2 SubE (genE a) (genE b)
genE (LtE a b) = liftM2 LtE (withty IntT $ genE a) (withty IntT $ genE b)
genE (GtE a b) = liftM2 GtE (withty IntT $ genE a) (withty IntT $ genE b)
genE (LeE a b) = liftM2 LeE (withty IntT $ genE a) (withty IntT $ genE b)
genE (GeE a b) = liftM2 GeE (withty IntT $ genE a) (withty IntT $ genE b)
genE (EqE a b) = withsamety EqE a b
genE (NeqE a b) = withsamety NeqE a b
genE (ArrayE a) = do
    ty <- asks gr_oty
    case ty of
        ArrT t _ -> ArrayE <$> withty t (mapM genE a)
        _ -> ArrayE <$> withty UnknownT (mapM genE a)
genE (XorE a b) = liftM2 XorE (genE a) (genE b)
genE (MulE a b) = liftM2 MulE (genE a) (genE b)
genE (ModE a b) = liftM2 ModE (genE a) (genE b)
genE (OrE a b) = liftM2 OrE (genE a) (genE b)
genE (LOrE a b) = liftM2 LOrE (withty BitT $ genE a) (withty BitT $ genE b)
genE (LAndE a b) = liftM2 LAndE (withty BitT $ genE a) (withty BitT $ genE b)
genE (ShlE a b) = liftM2 ShlE (genE a) (withty IntT $ genE b)
genE (ShrE a b) = liftM2 ShrE (genE a) (withty IntT $ genE b)
genE (NotE a) = NotE <$> genE a
genE (HoleE bnd) = do
  ty <- asks gr_oty
  env <- asks gr_env
  ValE <$> (liftSymbolic $ mkFreeArg env bnd ty)
genE (BitChooseE a b) = do
  env <- asks gr_env
  ty <- asks gr_oty
  -- TODO: use the bit width of a and b, not 32.
  x <- ValE <$> (liftSymbolic $ mkFreeArg env 32 ty)
  a' <- genE a
  b' <- genE b
  return (OrE (AndE a' x) (AndE b' (NotE x)))
genE x@(ValE (IntV v)) = do
    -- The front end uses IntE for integer literals, but they may not have
    -- type int. Change to the appropriate expression if a different type is
    -- expected here.
    -- TODO: this is a bit hackish. Can we clean it up please?
    env <- asks gr_env
    t <- asks gr_oty
    case evalT env t of
      IntT -> return x
      BitT -> return . ValE $ case v of
                                 0 -> BitV False
                                 1 -> BitV True
                                 _ -> error $ "literal " ++ show v ++ " is too big for bit type"
      ArrT t _ -> do
        x <- withty t (genE x)
        return $ ArrayE [x]
      _ -> return x   -- TODO: is it okay to default to int?
genE x@(ValE {}) = return x
genE x@(VarE {}) = return x
genE (AccessE a b) = do
    a' <- withty UnknownT $ genE a
    b' <- withty IntT (genE b)
    return (AccessE a' b')
genE (CastE t e) = CastE t <$> withty UnknownT (genE e)
genE (AppE fnm xs) = do
    env <- asks gr_env
    case Map.lookup fnm env of
       Just (FunD _ f kind) -> do
          let FunT _ txs = f_type f
          xs' <- sequence [withty t (genE x) | (t, x) <- zip txs xs]
          fnm' <- case kind of
                 GeneratorF -> do
                    generated <- genD (FunD fnm f NormalF)
                    emit generated
                 _ -> return fnm
          return $ AppE fnm' xs'

-- Generate a binary operator, where the only thing we know about the operand
-- types is that they must be the same.
withsamety :: (Expr -> Expr -> Expr) -> Expr -> Expr -> GM Expr
withsamety f a b = do
    t <- liftM2 unify (typeof a) (typeof b)
    liftM2 f (withty t $ genE a) (withty t $ genE b)

-- Determine as best as possible the type of the given expression.
-- Returns 'UnknownT' if the type is not certain.
typeof :: Expr -> GM Type
typeof (ValE v) = return $ typeofV v
typeof (AndE a b) = liftM2 unify (typeof a) (typeof b)
typeof (AddE a b) = liftM2 unify (typeof a) (typeof b)
typeof (SubE a b) = liftM2 unify (typeof a) (typeof b)
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
typeof (LOrE a b) = return BitT
typeof (LAndE a b) = return BitT
typeof (ShlE a b) = typeof a
typeof (ShrE a b) = typeof a
typeof (NotE a) = typeof a
typeof (HoleE bnd) = return UnknownT
typeof (BitChooseE a b) = liftM2 unify (typeof a) (typeof b)
typeof (VarE nm) = do
    env <- asks gr_env
    tyenv <- gets ts_tyenv
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
    env <- asks gr_env
    case Map.lookup nm env of
       Just (FunD _ (Function (FunT v _) _ _) _) -> return v
       Nothing -> return UnknownT

typeofV :: Value -> Type
typeofV (ArrayV []) = UnknownT
typeofV (ArrayV xs) = ArrT (typeofV (head xs)) (ValE (IntV (length xs)))
typeofV (BitV {}) = BitT
typeofV (BitsV b) = ArrT BitT (ValE (IntV $ width b))
typeofV (IntV v) = UnknownT -- could be any integer literal
typeofV (FunV f) = UnknownT


-- Try to unify the given types.
unify :: Type -> Type -> Type
unify UnknownT t = t
unify t UnknownT = t
unify (ArrT a x) (ArrT b _) = ArrT (unify a b) x
unify x _ = x

