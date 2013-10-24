
module Generate (generate) where

import Smten.Prelude
import Smten.Control.Monad.Reader
import Smten.Control.Monad.State
import Smten.Data.Functor
import qualified Smten.Data.Map as Map
import Smten.Data.Tuple
import Smten.Symbolic

import Bits
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
    let tyenv = Map.fromList (map swap . f_args . fd_val $ d)
        oty = f_outty . fd_val $ d
    tyenvold <- gets ts_tyenv
    modify $ \s -> s { ts_tyenv = tyenv }
    body' <- withty oty $ genS (f_body . fd_val $ d)
    let val' = (fd_val d) { f_body = body' }
    modify $ \s -> s { ts_tyenv = tyenvold }
    return $ d { fd_val = val' }

genS :: Stmt -> GM Stmt
genS (ReturnS x) = ReturnS <$> genE x
genS (AssertS x) = AssertS <$> withty BitT (genE x)
genS (RepeatS n s) = liftM2 RepeatS (withty IntT (genE n)) (genS s)
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
    Just ty -> UpdateS nm <$> (withty ty (genE e))
genS (ArrUpdateS nm idx e) = do
    idx' <- withty IntT $ genE idx
    e' <- withty BitT $ genE e
    return (ArrUpdateS nm idx' e')
genS (IfS p a b) = liftM3 IfS (withty BitT $ genE p) (genS a) (genS b)
genS (BlockS xs) = BlockS <$> mapM genS xs

genE :: Expr -> GM Expr
genE (AndE a b) = liftM2 AndE (genE a) (genE b)
genE (AddE a b) = liftM2 AddE (genE a) (genE b)
genE (SubE a b) = liftM2 SubE (genE a) (genE b)
genE (LtE a b) = withsamety LtE a b
genE (GtE a b) = withsamety GtE a b
genE (EqE a b) = withsamety EqE a b
genE (ArrayE a) = ArrayE <$> withty BitT (mapM genE a)
genE (XorE a b) = liftM2 XorE (genE a) (genE b)
genE (MulE a b) = liftM2 MulE (genE a) (genE b)
genE (OrE a b) = liftM2 OrE (genE a) (genE b)
genE (LOrE a b) = liftM2 LOrE (withty BitT $ genE a) (withty BitT $ genE b)
genE (LAndE a b) = liftM2 LAndE (withty BitT $ genE a) (withty BitT $ genE b)
genE (ShlE a b) = liftM2 ShlE (genE a) (withty IntT $ genE b)
genE (ShrE a b) = liftM2 ShrE (genE a) (withty IntT $ genE b)
genE (NotE a) = NotE <$> genE a
genE (HoleE bnd) = do
  ty <- asks gr_oty
  env <- asks gr_env
  liftSymbolic $ mkFreeArg env bnd ty
genE (BitChooseE a b) = do
  env <- asks gr_env
  ty <- asks gr_oty
  -- TODO: use the bit width of a and b, not 32.
  x <- liftSymbolic $ mkFreeArg env 32 ty
  a' <- genE a
  b' <- genE b
  return (OrE (AndE a' x) (AndE b' (NotE x)))
genE x@(BitE {}) = return x
genE x@(BitsE {}) = return x
genE x@(IntE v) = do
    -- The front end uses IntE for integer literals, but they may not have
    -- type int. Change to the appropriate expression if a different type is
    -- expected here.
    -- TODO: this is a bit hackish. Can we clean it up please?
    t <- asks gr_oty
    return $ case t of
               IntT -> x
               BitT -> case v of
                         0 -> BitE False
                         1 -> BitE True
                         _ -> error $ "literal " ++ show v ++ " is too big for bit type"
               BitsT (IntE w) -> BitsE (intB w v)
               _ -> x   -- TODO: is it okay to default to int?
genE x@(VarE {}) = return x
genE (AccessE a b) = do
    a' <- withty UnknownT $ genE a
    b' <- withty IntT (genE b)
    return (AccessE a' b')
genE (CastE t e) = CastE t <$> withty UnknownT (genE e)
genE (AppE f xs) = do
    env <- asks gr_env
    case f of
       VarE gnm | Just (FunD _ gf GeneratorF) <- Map.lookup gnm env -> do
           generated <- genD (FunD gnm gf NormalF)
           gnm' <- emit generated
           AppE (VarE gnm') <$> (withty UnknownT $ mapM genE xs)
       _ -> liftM2 AppE (withty UnknownT $ genE f)
                        (withty UnknownT $ mapM genE xs)
genE x@(FunE f) = return x

-- Generate a binary operator, where the only thing we know about the operand
-- types is that they must be the same.
withsamety :: (Expr -> Expr -> Expr) -> Expr -> Expr -> GM Expr
withsamety f a b = do
    t <- liftM2 unify (typeof a) (typeof b)
    liftM2 f (withty t $ genE a) (withty t $ genE b)

-- Determine as best as possible the type of the given expression.
-- Returns 'UnknownT' if the type is not certain.
typeof :: Expr -> GM Type
typeof (AndE a b) = liftM2 unify (typeof a) (typeof b)
typeof (AddE a b) = liftM2 unify (typeof a) (typeof b)
typeof (SubE a b) = liftM2 unify (typeof a) (typeof b)
typeof (LtE a b) = return BitT
typeof (GtE a b) = return BitT
typeof (EqE a b) = return BitT
typeof (ArrayE a) = return UnknownT
typeof (XorE a b) = liftM2 unify (typeof a) (typeof b)
typeof (MulE a b) = return IntT
typeof (OrE a b) = liftM2 unify (typeof a) (typeof b)
typeof (LOrE a b) = return BitT
typeof (LAndE a b) = return BitT
typeof (ShlE a b) = typeof a
typeof (ShrE a b) = typeof a
typeof (NotE a) = typeof a
typeof (HoleE bnd) = return UnknownT
typeof (BitChooseE a b) = liftM2 unify (typeof a) (typeof b)
typeof (BitE {}) = return BitT
typeof (BitsE b) = return (BitsT (IntE $ width b))
typeof (IntE v) = return UnknownT -- could be any integer literal
typeof (VarE nm) = do
    env <- asks gr_env
    tyenv <- gets ts_tyenv
    case Map.lookup nm tyenv of
        Just v -> return v
        Nothing -> case Map.lookup nm env of    
                      Just (VarD ty _ _) -> return ty
                      Just (FunD _ _ _) -> return UnknownT   -- TODO: can we have a function type which we can return?
                      Nothing -> return UnknownT
typeof (AccessE a b) = return BitT
typeof (CastE t e) = return t
typeof (AppE (VarE nm) xs) = do
    env <- asks gr_env
    case Map.lookup nm env of
       Just (FunD _ v _) -> return $ f_outty v
       Nothing -> return UnknownT
typeof (AppE f xs) = return UnknownT
typeof (FunE f) = return UnknownT


-- Try to unify the given types.
unify :: Type -> Type -> Type
unify UnknownT t = t
unify t UnknownT = t
unify x _ = x

