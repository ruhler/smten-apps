
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Static (
    static,
  ) where

import Smten.Prelude

import Smten.Control.Monad.Reader
import Smten.Control.Monad.State
import qualified Smten.Data.Map as Map
import Smten.Data.Functor

import Eval
import Program
import Ppr
import Syntax

-- Perform static analysis and evaluation on the given program.
-- Currently does the following: 
--   * evaluate all types.
--      In particular, the width of array types will be of
--      the form: ValE (IntV n)
--   * typechecks the program.
--      Reports an error if the program does not type check.
static :: Program -> Program
static p = do
  let readed = runReaderT (staticM p) (SR p (error "no top level output type"))
  evalState readed (SS Map.empty)

type TypeEnv = Map.Map Name Type

data SR = SR {
    -- | The program environment.
    sr_env :: Program,

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

instance Static Program where
   staticM p = program <$> mapM staticM (decls p)

instance Static Type where
   -- TODO: Assert StructT has an associated struct type declaration?
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

  staticM (StructD nm fields) =
      StructD nm <$> mapM staticM fields

instance Static (Name, Type) where
  staticM (nm, ty) = do
     ty' <- staticM ty
     return (nm, ty')

instance Static Arg where
  staticM (Arg nm ty isref) = do
    ty' <- staticM ty
    return $ Arg nm ty' isref

instance Static Function where
  staticM (Function ty args body) = do
     ty' <- staticM ty
     args' <- mapM staticM args
     let tyenv = Map.fromList [(nm, ty) | Arg nm ty _ <- args']
     tyenvold <- gets ss_tyenv
     modify $ \s -> s { ss_tyenv = tyenv }
     body' <- withty ty' $ staticM body
     modify $ \s -> s { ss_tyenv = tyenvold }
     return $ Function ty' args' body'

instance Static Stmt where
  staticM (ReturnS x) = ReturnS <$> staticM x
  staticM (ExprS x) = do
     ty <- typeof x
     ExprS <$> withty ty (staticM x)
  staticM (AssertS x) = AssertS <$> withty BitT (staticM x)
  staticM (ReorderS xs) = ReorderS <$> mapM staticM xs
  staticM (RepeatS n s) = liftM2 RepeatS (withty IntT $ staticM n) (staticM s)
  staticM (WhileS c s) = liftM2 WhileS (withty BitT $ staticM c) (staticM s)
  staticM (ForS init cond incr body) =
    liftM4 ForS (staticM init) (withty BitT (staticM cond)) (staticM incr) (staticM body)
  staticM (DeclS ty vars) = do
    ty' <- staticM ty
    let svars (nm, mval) = do
          env <- gets ss_tyenv
          let env' = Map.insert nm ty' env
          modify $ \s -> s { ss_tyenv = env' }
          mval' <- case mval of
                      Nothing -> return Nothing
                      Just e -> Just <$> withty ty' (staticM e)
          return (nm, mval')
    DeclS ty' <$> mapM svars vars

  staticM (UpdateS lv e) = do
    (lv', ty) <- staticLV lv
    UpdateS lv' <$> (withty ty (staticM e))

  staticM (IfS p a b) = liftM3 IfS (withty BitT $ staticM p) (staticM a) (staticM b)
  staticM (BlockS xs) = blockS <$> mapM staticM xs

instance Static LVal where
    staticM x = fst <$> staticLV x

-- Return the LVal with static check done, and return the type of 
-- expression expected.
staticLV :: LVal -> SM (LVal, Type)
staticLV lv@(VarLV nm) = do
    env <- asks sr_env
    tyenv <- gets ss_tyenv
    ty <- case Map.lookup nm tyenv of
            Just ty -> return ty
            Nothing ->
              case Map.lookup nm env of
                Just (VarD ty _ _) -> return ty
                Nothing -> error $ "variable " ++ nm ++ " not in scope"
    ty' <- staticM ty
    return (lv, ty')
staticLV (ArrLV lv idx) = do
    (lv', arrt) <- staticLV lv
    t <- case arrt of
                ArrT t _ -> staticM t
                _ -> error $ "expected array type, found " ++ show arrt
    idx' <- withty IntT $ staticM idx
    return (ArrLV lv' idx', t)
staticLV (BulkLV lv lo w) = do
    (lv', t) <- staticLV lv
    lo' <- withty IntT $ staticM lo
    w' <- withty IntT $ staticM w
    case (w', t) of
        (ValE (IntV wv), ArrT et (ValE (IntV w)))
           | wv <= w -> return (BulkLV lv' lo' w', ArrT et (ValE (IntV wv)))
           | otherwise -> error $ "invalid bounds for bulk update of width: " ++ show wv
        (_, ArrT {}) -> error $ "bulk update width could not be determined statically: " ++ show (w')
        (_, _) -> error $ "expected array type, but found " ++ show t
staticLV (FieldLV lv m) = do
    (lv', t) <- staticLV lv
    case t of
      StructT nm -> do
        env <- asks sr_env
        case Map.lookup nm env of
           Just (StructD _ fields)
            | Just to <- lookup m fields -> return (FieldLV lv' m, to)
            | otherwise -> error $ m ++ " is not a field of struct type " ++ nm
           _ -> error $ nm ++ " does not a struct type"
      _ -> error $ "field update expected struct type, but found type: " ++ show t
staticLV (ChoiceLV a b) = do
    (lva, ta) <- staticLV a
    (lvb, tb) <- staticLV b
    if (ta == tb)
       then return ()
       else error $ "types don't match in LV choice: " ++ show ta ++ " vs. " ++ show tb
    return (ChoiceLV lva lvb, ta)
 


instance Static Expr where
  staticM e = do
    dst <- asks sr_oty
    src <- typeof e
    case () of
     _ | src `matches` dst -> staticE e
       | src `subtype` dst -> ICastE dst <$> (withty src $ staticE e)
       | otherwise -> error $ "[000] expected type " ++ show dst ++ " but found type " ++ show src ++ " in the expression " ++ show e

staticE :: Expr -> SM Expr
staticE (BinaryE AndOp a b) = bitwiseop AndOp a b
staticE (BinaryE LAndOp a b) = bitwiseop LAndOp a b

staticE (BinaryE AddOp a b) = do
  ty <- asks sr_oty
  case ty of
    IntT -> return ()
    ArrT BitT _ -> return ()
    _ -> error $ "unsupported type for addition: " ++ show ty
  liftM2 (BinaryE AddOp) (staticM a) (staticM b)

staticE (BinaryE SubOp a b) = intop SubOp (-) a b
staticE (BinaryE LtOp a b) = cmpop LtOp a b
staticE (BinaryE GtOp a b) = cmpop GtOp a b
staticE (BinaryE LeOp a b) = cmpop LeOp a b
staticE (BinaryE GeOp a b) = cmpop GeOp a b
staticE (BinaryE EqOp a b) = eqop EqOp a b
staticE (BinaryE NeqOp a b) = eqop NeqOp a b
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

staticE (BinaryE XorOp a b) = bitwiseop XorOp a b
staticE (BinaryE MulOp a b) = intop MulOp (*) a b
staticE (BinaryE ModOp a b) = intop ModOp rem a b
staticE (BinaryE DivOp a b) = intop DivOp quot a b
staticE (BinaryE OrOp a b) = bitwiseop OrOp a b
staticE (BinaryE LOrOp a b) = bitwiseop LOrOp a b
staticE (BinaryE ShlOp a b) = shiftop ShlOp a b
staticE (BinaryE ShrOp a b) = shiftop ShrOp a b
staticE (PostIncrE a) = incrop PostIncrE "post ++" a
staticE (PostDecrE a) = incrop PostDecrE "post --" a
staticE (PreIncrE a) = incrop PreIncrE "pre ++" a
staticE (PreDecrE a) = incrop PreDecrE "pre --" a
staticE (PlusEqE a b) = do
  ty <- asks sr_oty
  case ty of
    IntT -> return ()
    _ -> error $ "unsupported type for +=:" ++ show ty
  liftM2 PlusEqE (staticM a) (staticM b)

staticE (BitChooseE _ a b) = do
  ty <- asks sr_oty
  case ty of
    BitT -> return ()
    ArrT BitT _ -> return ()
    _ -> error $ "unsupported type for bitwise {|}: " ++ show ty
  liftM2 (BitChooseE ty) (staticM a) (staticM b)

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
staticE (ChoiceE a b) = liftM2 ChoiceE (staticE a) (staticE b)
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

staticE (FieldE x m) = do
  st <- typeof x
  oty <- asks sr_oty
  case st of
    StructT nm -> do
      env <- asks sr_env
      case Map.lookup nm env of
        Just (StructD _ fields)
          | Just to <- lookup m fields -> 
              if to == oty
                 then do
                   x' <- withty st (staticM x)
                   return $ FieldE x' m
                 else error $ "expected type " ++ show oty ++ ", but found type: " ++ show to
          | otherwise -> error $ nm ++ " does not name a struct type"
    _ -> error $ "expected struct, but got something of type: " ++ show st

staticE (NewE nm initlist) = do
  -- Verify this has the type we expect
  oty <- asks sr_oty
  if oty == StructT nm
    then return ()
    else error $ "expected type " ++ show oty ++ " but found type: " ++ show (StructT nm)

  -- Verify the struct type has been declared,
  -- and get the field types
  env <- asks sr_env
  fieldtypes <- case Map.lookup nm env of
                    Just (StructD _ fields) -> return fields
                    _ -> error $ nm ++ " does not name a struct type"

  -- Statically evaluate each field in the init list
  let staticF (n, e) =
        case lookup n fieldtypes of
          Just ty -> do
            e' <- withty ty $ staticM e
            return (n, e')
          Nothing -> error $ nm ++ " does not contain a member named " ++ show n
  initlist' <- mapM staticF initlist
  return (NewE nm initlist')

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
        FunT foty txs <- staticM $ functionT f
        if oty == foty
           then return ()
           else error $ "[004]expected type " ++ show oty ++ " but found type: " ++ show foty

        if length txs == length xs
           then return ()
           else error $ "wrong number of arguments passed to function " ++ fnm

        xs' <- sequence [withty t (staticM x) | (t, x) <- zip txs xs]

        -- Verify all reference arguments can be converted to LVals.
        let lfcheck :: Arg -> Expr -> SM ()
            lfcheck (Arg _ _ False) _ = return ()
            lfcheck (Arg nm _ True) x =
               case asLVal x of
                  Just _ -> return ()
                  Nothing -> error $ "can't pass " ++ show x ++ " by reference"
        zipWithM_ lfcheck (f_args f) xs'

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

  staticM VoidV = do
    ty <- asks sr_oty
    case ty of
        VoidT {} -> return VoidV
        _ -> error $ "expected type " ++ show ty ++ " but got void"

  staticM x@(PointerV {}) = do
    ty <- asks sr_oty
    case ty of
      StructT {} -> return x
      _ -> error $ "expected type " ++ show ty ++ " but got a pointer"
      

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
unify (StructT "") (StructT nm) = StructT nm
unify (StructT nm) (StructT "") = StructT nm
unify (ArrT a (ValE (IntV wa))) (ArrT b (ValE (IntV wb)))
  = ArrT (unify a b) (ValE (IntV (max wa wb)))
unify a@(ArrT {}) b@(ArrT {})
  = error $ "unable to unify array types: " ++ show (a, b)
unify (ArrT a wa) b = unify (ArrT a wa) (ArrT b (ValE (IntV 1)))
unify a (ArrT b wb) = unify (ArrT b (ValE (IntV 1))) (ArrT b wb)
unify a b = error $ "unable to unify types: " ++ show (a, b)

-- Given types A and B,
--  return True if B = A after substition of UnknownT types in A.
matches :: Type -> Type -> Bool
matches a b | a == b = True
matches UnknownT _ = True
matches (FunT a as) (FunT b bs) = and (zipWith matches (a:as) (b:bs))
matches (ArrT a (ValE wa)) (ArrT b (ValE wb)) = wa == wb && a `matches` b
matches (StructT "") (StructT {}) = True
matches _ _ = False

-- Comparison operators: <, >, <=, >=
-- Typing Rules:
--   * Return type is bit
--   * Argument types are int.
cmpop :: BinOp -> Expr -> Expr -> SM Expr
cmpop op a b = do
    ty <- asks sr_oty
    case ty of
      BitT -> return ()
      _ -> error $ "unsupported return type for " ++ pretty op ++ " operator: " ++ show ty
    liftM2 (BinaryE op) (withty IntT $ staticM a) (withty IntT $ staticM b)


-- Equality operators: == and !=
-- Typing Rules:
--   * Return type is bit
--   * Argument types are the same.
eqop :: BinOp -> Expr -> Expr -> SM Expr
eqop op a b = do
    ty <- asks sr_oty
    case ty of
      BitT -> return ()
      _ -> error $ "unsupported return type for " ++ pretty op ++ " operator: " ++ show ty
    -- We need to determine the argument types here.
    -- See if we can infer them from the arguments.
    targ <- liftM2 unify (typeof a) (typeof b)
    let eqtycheck UnknownT = error $ "ambiguous type for " ++ pretty op ++ " operator"
        eqtycheck BitT = return ()
        eqtycheck IntT = return ()
        eqtycheck (ArrT ty _) = eqtycheck ty
        eqtycheck (StructT {}) = return ()
        eqtycheck _ = error $ "TODO: does " ++ pretty op ++ " operator support type: " ++ show targ
    eqtycheck targ
    liftM2 (BinaryE op) (withty targ $ staticM a) (withty targ $ staticM b)

-- Bitwise Operators
-- Typing Rules:
--   * The return type is either bit or bit[]
--   * The argument types match the return type.
bitwiseop :: BinOp -> Expr -> Expr -> SM Expr
bitwiseop op a b = do
    ty <- asks sr_oty
    case ty of
      BitT -> return ()
      ArrT BitT _ -> return ()
      _ -> error $ "unsupported type for bitwise " ++ pretty op ++ ": " ++ show ty
    liftM2 (BinaryE op) (staticM a) (staticM b)

-- Integer operators
-- Typing Rules:
--   * The return type is int
--   * The argument types are int
intop :: BinOp -> (Int -> Int -> Int) -> Expr -> Expr -> SM Expr
intop op f a b = do
    ty <- asks sr_oty
    case ty of
      IntT -> return ()
      _ -> error $ "unsupported type for operator " ++ pretty op ++ ": " ++ show ty
    a' <- staticM a
    b' <- staticM b
    case (a', b') of
       (ValE (IntV av), ValE (IntV bv)) -> return $ ValE (IntV (f av bv))
       _ -> return $ BinaryE op a' b'

-- Shift operators
-- Typing Rules:
--   * The return type is bit[] or int.
--   * The first argument type matches the return type
--   * The second argument type is Int.
shiftop :: BinOp -> Expr -> Expr -> SM Expr
shiftop op a b = do
    ty <- asks sr_oty
    ty' <- case ty of
              ArrT BitT _ -> return ty
              IntT -> return ty
              _ -> error $ "unsupported type for operator " ++ pretty op ++ ": " ++ show ty
    liftM2 (BinaryE op) (withty ty $ staticM a) (withty IntT $ staticM b)

-- return the type of a shift expression given its first argument.
typeofshift :: Expr -> SM Type
typeofshift a = do
  ty <- typeof a
  return $ case ty of
             ArrT BitT _ -> ty
             IntT -> ty
             _ -> UnknownT

incrop :: (LVal -> Expr) -> String -> LVal -> SM Expr
incrop f nm a = do
  ty <- asks sr_oty
  case ty of
    IntT -> return ()
    _ -> error $ "unsupported type for " ++ nm ++ ": " ++ show ty
  f <$> staticM a

-- Determine as best as possible the type of the given expression.
-- Returns 'UnknownT' if the type is not certain.
typeof :: Expr -> SM Type
typeof (ValE v) = return $ typeofV v
typeof (BinaryE AndOp a b) = liftM2 unify (typeof a) (typeof b)
typeof (BinaryE LAndOp a b) = liftM2 unify (typeof a) (typeof b)
typeof (BinaryE AddOp a b) = liftM2 unify (typeof a) (typeof b)
typeof (BinaryE SubOp a b) = return IntT
typeof (BinaryE LtOp a b) = return BitT
typeof (BinaryE GtOp a b) = return BitT
typeof (BinaryE LeOp a b) = return BitT
typeof (BinaryE GeOp a b) = return BitT
typeof (BinaryE EqOp a b) = return BitT
typeof (BinaryE NeqOp a b) = return BitT
typeof (ArrayE a) = do
   ty <- foldr unify UnknownT <$> mapM typeof a
   return $ ArrT ty (ValE (IntV (length a)))
typeof (BinaryE XorOp a b) = liftM2 unify (typeof a) (typeof b)
typeof (BinaryE MulOp a b) = return IntT
typeof (BinaryE ModOp a b) = return IntT
typeof (BinaryE DivOp a b) = return IntT
typeof (CondE _ a b) = liftM2 unify (typeof a) (typeof b)
typeof (BinaryE OrOp a b) = liftM2 unify (typeof a) (typeof b)
typeof (BinaryE LOrOp a b) = liftM2 unify (typeof a) (typeof b)
typeof (BinaryE ShlOp a b) = typeofshift a
typeof (BinaryE ShrOp a b) = typeofshift a
typeof (PostIncrE a) = snd <$> staticLV a
typeof (PostDecrE a) = snd <$> staticLV a
typeof (PreIncrE a) = snd <$> staticLV a
typeof (PreDecrE a) = snd <$> staticLV a
typeof (PlusEqE a b) = liftM2 unify (snd <$> staticLV a) (typeof b)
typeof (NotE a) = typeof a
typeof (HoleE ty _) = return ty
typeof (ChoiceE a b) = liftM2 unify (typeof a) (typeof b)
typeof (BitChooseE _ a b) = liftM2 unify (typeof a) (typeof b)
typeof (VarE nm) = do
    env <- asks sr_env
    tyenv <- gets ss_tyenv
    case Map.lookup nm tyenv of
        Just v -> staticM v
        Nothing -> case Map.lookup nm env of    
                      Just d -> staticM $ declT d
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
typeof (FieldE x m) = do
  tx <- typeof x
  case tx of
    StructT nm -> do
      env <- asks sr_env
      case Map.lookup nm env of
        Just (StructD _ fields) | Just t <- lookup m fields -> return t
        _ -> return UnknownT
    _ -> return UnknownT
typeof (NewE nm _) = return (StructT nm)
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
subtype BitT IntT = True
subtype (ArrT ta (ValE (IntV wa))) (ArrT tb (ValE (IntV wb)))
  | ta `subtype` tb && wa <= wb = True
  | ta `matches` tb && wa < wb = True
subtype (ArrT a wa) (ArrT b wb) = error "subtype: unknown array widths"
subtype a b@(ArrT {}) = subtype (ArrT a (ValE (IntV 1))) b
subtype _ _ = False

