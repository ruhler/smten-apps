
module Generate (generate) where

import Smten.Prelude
import Smten.Control.Monad.Reader
import Smten.Control.Monad.State
import Smten.Data.Functor
import qualified Smten.Data.Map as Map
import Smten.Symbolic

import Input
import Sketch

-- Given a program with explicit holes, generate a corrisponding symbolic
-- candidate program without holes.
generate :: ProgEnv -> Symbolic Prog
generate p = do
  let readed = runReaderT (mapM genD (declsof p)) (GR p)
  (ds, s) <- runStateT readed (TS [])
  return $ ts_decls s ++ ds

type TypeEnv = Map.Map Name Type

data GR = GR {
    -- | The program environment.
    gr_env :: ProgEnv
}

data TS = TS {
    -- | Emmitted (generated) declarations
    ts_decls :: [Decl]
}

type GM = ReaderT GR (StateT TS Symbolic)

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
    let FunT _ argtys = f_type . fd_val $ d
        tyenv = Map.fromList (zip (f_args . fd_val $ d) argtys)
    body' <- genS (f_body . fd_val $ d)
    let val' = (fd_val d) { f_body = body' }
    return $ d { fd_val = val' }

genS :: Stmt -> GM Stmt
genS (ReturnS x) = ReturnS <$> genE x
genS (AssertS x) = AssertS <$> genE x
genS (RepeatS en s) = do
  -- TODO: evaluate n statically as much as possible here
  -- Perhaps a 'simplify' operation on expressions would be useful
  --
  -- TODO: what if the statement changes the condition? How do we keep track
  -- of the original condition?
  let count = case en of
                ValE (IntV v) -> v
                _ -> bnd_unroll_amnt
  en' <- genE en
  let unroll n
        | n >= count = blockS []
        | otherwise =
            let ifthen = blockS [s, unroll (n+1)]
                ifelse = (blockS [])
            in IfS (GtE en' (ValE (IntV n))) ifthen ifelse
  genS (unroll 0)

genS (WhileS c s) = liftM2 WhileS (genE c) (genS s)
genS (ForS init cond incr body) =
    liftM4 ForS (genS init) (genE cond) (genS incr) (genS body)
genS s@(DeclS {}) = return s
genS (UpdateS nm e) = UpdateS nm <$> genE e
genS (ArrUpdateS nm idx e) = liftM2 (ArrUpdateS nm) (genE idx) (genE e)
genS (ArrBulkUpdateS nm lo hi e) = liftM3 (ArrBulkUpdateS nm) (genE lo) (genE hi) (genE e)
genS (IfS p a b) = liftM3 IfS (genE p) (genS a) (genS b)
genS (BlockS xs) = blockS <$> mapM genS xs

genE :: Expr -> GM Expr
genE (AndE a b) = liftM2 AndE (genE a) (genE b)
genE (AddE a b) = liftM2 AddE (genE a) (genE b)
genE (SubE a b) = liftM2 SubE (genE a) (genE b)
genE (LtE a b) = liftM2 LtE (genE a) (genE b)
genE (GtE a b) = liftM2 GtE (genE a) (genE b)
genE (LeE a b) = liftM2 LeE (genE a) (genE b)
genE (GeE a b) = liftM2 GeE (genE a) (genE b)
genE (EqE a b) = liftM2 EqE (genE a) (genE b)
genE (NeqE a b) = liftM2 NeqE (genE a) (genE b)
genE (ArrayE a) = ArrayE <$> mapM genE a
genE (XorE a b) = liftM2 XorE (genE a) (genE b)
genE (MulE a b) = liftM2 MulE (genE a) (genE b)
genE (ModE a b) = liftM2 ModE (genE a) (genE b)
genE (OrE a b) = liftM2 OrE (genE a) (genE b)
genE (ShlE a b) = liftM2 ShlE (genE a) (genE b)
genE (ShrE a b) = liftM2 ShrE (genE a) (genE b)
genE (NotE a) = NotE <$> genE a
genE (HoleE ty bnd) = ValE <$> (liftSymbolic $ mkFreeArg bnd ty)
genE (BitChooseE ty a b) = do
  -- TODO: use the bit width of a and b, not 32.
  x <- ValE <$> (liftSymbolic $ mkFreeArg 32 ty)
  a' <- genE a
  b' <- genE b
  return (OrE (AndE a' x) (AndE b' (NotE x)))
genE x@(ValE {}) = return x
genE x@(VarE {}) = return x
genE (AccessE a b) = liftM2 AccessE (genE a) (genE b)
genE (CastE t e) = CastE t <$> genE e
genE (ICastE s d e) = ICastE s d <$> genE e
genE (AppE fnm xs) = do
    env <- asks gr_env
    case Map.lookup fnm env of
       Just (FunD _ f kind) -> do
          xs' <- mapM genE xs
          fnm' <- case kind of
                 GeneratorF -> do
                    generated <- genD (FunD fnm f NormalF)
                    emit generated
                 _ -> return fnm
          return $ AppE fnm' xs'

