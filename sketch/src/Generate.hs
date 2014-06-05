
module Generate (generate) where

import Smten.Prelude
import Smten.Control.Monad.Reader
import Smten.Control.Monad.State
import Smten.Data.Functor
import Smten.Data.List
import qualified Smten.Data.Map as Map
import Smten.Data.Maybe
import Smten.Search

import Input
import Options
import Program
import Syntax

-- Given a program with explicit holes, generate a corresponding symbolic
-- candidate program without holes.
generate :: Options -> Program -> Space Program
generate opts p = do
  let readed = runReaderT (mapM genD (decls p)) (GR opts p Map.empty)
  (ds, s) <- runStateT readed (TS [] 0)
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
    ts_decls :: [Decl],
    ts_fresh :: Int
}

type GM = ReaderT GR (StateT TS Space)

-- Emit a declaration. Use a template for the declaration name, and a
-- uniquified version of that name will be used instead and returned. 
emit :: Decl -> GM Name
emit d = do
    s <- get
    let nm = declN d ++ "_" ++ show (length $ ts_decls s)
    put $ s { ts_decls = d { declN = nm } : ts_decls s }
    return nm

-- Return a fresh variable name.
fresh :: GM Name
fresh = do
  s <- get
  let nm = "__x" ++ show (ts_fresh s)
  put $ s { ts_fresh = ts_fresh s + 1 }
  return nm

liftSpace :: Space a -> GM a
liftSpace = lift . lift

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
  matrix <- liftSpace $ mapM (const mkrow) xs'

  -- Each row and col should have exactly one statement executed
  let rowselects = map (map fst) matrix
      cols [] = []
      cols ([]:_) = []
      cols xs = map head xs : cols (map tail xs)
      colselects = map (map fst) (cols matrix)
  liftSpace $ guard (all oneset rowselects && all oneset colselects)

  -- Get and return the list of statements to execute
  let stmts = map (\(x, s) -> if x then s else BlockS []) (concat matrix)
  return $ blockS stmts

-- Note: The body of the expression is repeated before generation occurs in
-- the body.
-- TODO: evaluate n statically as much as possible here.
genS (RepeatS (ValE (IntV v)) s) = genS $ blockS (genericReplicate v s) 
genS (RepeatS e s) = do
  -- We don't know the repeat, so repeat it at most bnd_unroll_amnt times.
  -- We form the following program:
  --  int __x = e;
  --  if (__x >= 0) s
  --  if (__x >= 1) s
  --  ...
  --  if (__x >= 8) s
  opts <- asks gr_opts
  x <- fresh
  let bnd = bnd_unroll_amnt opts
      unroll n
        | n >= bnd = blockS []
        | otherwise =
            let ifthen = blockS [s, unroll (n+1)]
                ifelse = (blockS [])
            in IfS (BinaryE GtOp (VarE x) (ValE (intV n))) ifthen ifelse
  genS $ blockS [DeclS IntT [(x, Just e)], unroll 0]

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
genLV (ChoiceLV a b) = do
  a' <- genLV a
  b' <- genLV b
  liftSpace $ mplus (return a') (return b')

genOp :: BinOp -> GM BinOp
genOp (ChoiceOp xs) = do
  ops <- mapM genOp xs
  liftSpace $ msum (map return ops)
genOp op = return op

genE :: Expr -> GM Expr
genE (BinaryE op a b) = liftM3 BinaryE (genOp op) (genE a) (genE b)
genE (ArrayE a) = ArrayE <$> mapM genE a
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
  ValE <$> (liftSpace $ mkFreeArg bnd ty)
genE (ChoiceE a b) = do
  a' <- genE a
  b' <- genE b
  liftSpace $ mplus (return a') (return b')
genE (BitChooseE ty a b) = do
  -- TODO: use the bit width of a and b, not 32.
  x <- ValE <$> (liftSpace $ mkFreeArg 32 ty)
  a' <- genE a
  b' <- genE b
  return (BinaryE OrOp (BinaryE AndOp a' x) (BinaryE AndOp b' (NotE x)))
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
