
module Generate (generate) where

import Smten.Prelude
import Smten.Control.Monad.Reader
import Smten.Control.Monad.State
import Smten.Data.Functor
import qualified Smten.Data.Map as Map
import Smten.Data.Maybe
import Smten.Symbolic

import Input
import Options
import Program
import Syntax

-- Given a program with explicit holes, generate a corrisponding symbolic
-- candidate program without holes.
generate :: Options -> Program -> Symbolic Program
generate opts p = do
  let readed = runReaderT (mapM genD (decls p)) (GR opts p Map.empty)
  (ds, s) <- runStateT readed (TS [])
  return $ program (ts_decls s ++ ds)

type TypeEnv = Map.Map Name Type

data GR = GR {
    gr_opts :: Options,

    -- | The program environment.
    gr_env :: Program,

    -- | The call stack: used to bound recursion properly.
    gr_stack :: Map.Map Name Int
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
    let nm = declN d ++ "_" ++ show (length $ ts_decls s)
    put $ s { ts_decls = d { declN = nm } : ts_decls s }
    return nm

liftSymbolic :: Symbolic a -> GM a
liftSymbolic = lift . lift

genD :: Decl -> GM Decl
genD d@(VarD {}) = return d
genD d@(FunD _ _ GeneratorF) = return d
genD d@(FunD {}) = do
    let tyenv = Map.fromList [(nm, ty) | Arg nm ty _ <- f_args . fd_val $ d]
    body' <- genS (f_body . fd_val $ d)
    let val' = (fd_val d) { f_body = body' }
    return $ d { fd_val = val' }
genD d@(StructD {}) = return d

genS :: Stmt -> GM Stmt
genS (ReturnS x) = ReturnS <$> genE x
genS (ExprS x) = ExprS <$> genE x
genS (AssertS x) = AssertS <$> genE x
genS (ReorderS xs) = do
  -- We create a matrix of boolean variables x[i,j]
  -- x[i,j] is true if the ith statement executed is the jth statement in
  -- the given list.
  xs' <- mapM genS xs
  let mkelem s = do
        x <- free_Bool
        return (x, s)
      mkrow = mapM mkelem xs'
  matrix <- liftSymbolic $ mapM (const mkrow) xs'

  -- Each row and col should have exactly one statement executed
  let rowselects = map (map fst) matrix
      cols [] = []
      cols ([]:_) = []
      cols xs = map head xs : cols (map tail xs)
      colselects = map (map fst) (cols matrix)
  liftSymbolic $ assert (all oneset rowselects && all oneset colselects)

  -- Get and return the list of statements to execute
  let stmts = map (\(x, s) -> if x then s else BlockS []) (concat matrix)
  return $ blockS stmts

genS (RepeatS en s) = do
  -- TODO: evaluate n statically as much as possible here
  -- Perhaps a 'simplify' operation on expressions would be useful
  --
  -- TODO: what if the statement changes the condition? How do we keep track
  -- of the original condition?
  opts <- asks gr_opts
  let count = case en of
                ValE (IntV v) -> v
                _ -> bnd_unroll_amnt opts
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
genS (DeclS ty vars) = do
  let genVar v@(nm, Nothing) = return v
      genVar (nm, Just e) = do
        e' <- genE e    
        return (nm, Just e')
  vars' <- mapM genVar vars
  return (DeclS ty vars')
genS (UpdateS lv e) = liftM2 UpdateS (genLV lv) (genE e)
genS (IfS p a b) = liftM3 IfS (genE p) (genS a) (genS b)
genS (BlockS xs) = blockS <$> mapM genS xs

genLV :: LVal -> GM LVal
genLV v@(VarLV nm) = return v
genLV (ArrLV lv x) = liftM2 ArrLV (genLV lv) (genE x)
genLV (BulkLV lv lo w) = liftM3 BulkLV (genLV lv) (genE lo) (genE w)
genLV (FieldLV lv m) = do
  lv' <- genLV lv
  return $ FieldLV lv' m

genE :: Expr -> GM Expr
genE (AndE a b) = liftM2 AndE (genE a) (genE b)
genE (LAndE a b) = liftM2 LAndE (genE a) (genE b)
genE (OrE a b) = liftM2 OrE (genE a) (genE b)
genE (LOrE a b) = liftM2 LOrE (genE a) (genE b)
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
genE (DivE a b) = liftM2 DivE (genE a) (genE b)
genE (ShlE a b) = liftM2 ShlE (genE a) (genE b)
genE (ShrE a b) = liftM2 ShrE (genE a) (genE b)
genE (PostIncrE lv) = PostIncrE <$> genLV lv
genE (PostDecrE lv) = PostDecrE <$> genLV lv
genE (PreIncrE lv) = PreIncrE <$> genLV lv
genE (PreDecrE lv) = PreDecrE <$> genLV lv
genE (PlusEqE lv a) = liftM2 PlusEqE (genLV lv) (genE a)
genE (NotE a) = NotE <$> genE a
genE (CondE p a b) = liftM3 CondE (genE p) (genE a) (genE b)
genE (HoleE ty mbnd) = do
  opts <- asks gr_opts
  let bnd = fromMaybe (bnd_cbits opts) mbnd
  ValE <$> (liftSymbolic $ mkFreeArg bnd ty)
genE (BitChooseE ty a b) = do
  -- TODO: use the bit width of a and b, not 32.
  x <- ValE <$> (liftSymbolic $ mkFreeArg 32 ty)
  a' <- genE a
  b' <- genE b
  return (OrE (AndE a' x) (AndE b' (NotE x)))
genE x@(ValE {}) = return x
genE x@(VarE {}) = return x
genE (AccessE a b) = liftM2 AccessE (genE a) (genE b)
genE (BulkAccessE a b c) = liftM3 BulkAccessE (genE a) (genE b) (genE c)
genE (FieldE x nm) = do
   x' <- genE x
   return (FieldE x' nm)
genE x@(NewE nm fields) = do
  let genF (n, e) = do
         e' <- genE e
         return (n, e')
  NewE nm <$> mapM genF fields
genE (CastE t e) = CastE t <$> genE e
genE (ICastE d e) = ICastE d <$> genE e
genE (AppE fnm xs) = do
    xs' <- mapM genE xs
    gr <- ask
    let fcnt = fromMaybe 0 $ Map.lookup fnm (gr_stack gr)
    case Map.lookup fnm (gr_env gr) of
      Just (FunD _ f kind) -> do
         fnm' <- case (kind, fcnt < bnd_inline_amnt (gr_opts gr)) of
                    (GeneratorF, True) ->
                       local (\g -> g { gr_stack = Map.insert fnm (fcnt + 1) (gr_stack g) }) $ do
                          genD (FunD fnm f NormalF) >>= emit
                    (_, True) -> return fnm
                    (_, False) -> do
                        let f' = f { f_body = AssertS (ValE (BitV False)) }
                        genD (FunD fnm f' NormalF) >>= emit
         return $ AppE fnm' xs'

-- Return True if exactly one of the inputs is set.
oneset :: [Bool] -> Bool
oneset =
  -- s - at least some bit has been seen set.
  -- m - multiple bits have been seen set.
  let oneset' s m [] = s && not m
      oneset' s m (x:xs) = oneset' (s || x) ((s && x) || m) xs
  in oneset' False False
