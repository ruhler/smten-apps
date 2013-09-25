
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}
module Typing (tinferP) where

import Smten.Prelude
import qualified Smten.Data.Map as Map
import Smten.Control.Monad.State
import Smten.Data.Functor

import Bits
import Sketch

type TypeEnv = Map.Map Name Type

data TS = TS {
    -- | The type environment.
    ts_env :: TypeEnv,

    -- | The target output type.
    ts_oty :: Type
}

type TM = State TS



-- Perform type inference and checking on the given program.
-- Returns the type inferred program, or goes BOOM if there's a type error.
-- TODO: We probably ought to handle errors in a nicer way.
-- TODO: take the specs map as input rather than duplicate the work to create
-- it here and in synthesis?
tinferP :: Prog -> Prog
tinferP p = 
  let specs = Map.fromList [(fd_name x, x) | x <- p]
  in map (tinferD specs) p

tinferD :: Map.Map Name Decl -> Decl -> Decl
tinferD specs d =
   let m = Map.fromList (map (\(a, b) -> (b, a)) $ fd_args d) 
       stmts' = evalState (mapM tinferS (fd_stmts d)) (TS m (fd_outty d))
   in d { fd_stmts = stmts' }

tinferS :: Stmt -> TM Stmt
tinferS (ReturnS x) = do
    oty <- gets ts_oty
    env <- gets ts_env
    return (ReturnS (tinferE env oty x))
tinferS (DeclS ty nm) = do
    oty <- gets ts_oty
    env <- gets ts_env
    let env' = Map.insert nm ty env
    modify $ \s -> s { ts_env = env' }
    return (DeclS ty nm)
tinferS (UpdateS nm e) = do
    env <- gets ts_env
    case Map.lookup nm env of
        Nothing -> error $ "variable " ++ nm ++ " not declared"
        Just ty -> return $ UpdateS nm (tinferE env ty e)
tinferS (ArrUpdateS nm i e) = do
    env <- gets ts_env
    case Map.lookup nm env of
        Nothing -> error $ "variable " ++ nm ++ " not declared"
        Just _ -> do
          let i' = tinferE env IntT i
              e' = tinferE env BitT e
          return $ ArrUpdateS nm i' e'
tinferS (IfS p a b) = do
    env <- gets ts_env
    let p' = tinferE env BitT p
    a' <- tinferS a
    b' <- tinferS b
    return (IfS p' a' b')
tinferS (BlockS xs) = BlockS <$> mapM tinferS xs

-- tinferE tyenv tytgt x
--   tyenv - the type environment
--   tytgt - the asserted type of this expression
--   x - the expression to run inference on
tinferE :: TypeEnv -> Type -> Expr -> Expr
tinferE m t x =
  case x of
    -- TODO: for AndE and OrE and NotE: verify the target type is bit or
    -- bit[n] or whatever else it can be.
    AndE a b -> AndE (tinferE m t a) (tinferE m t b)
    OrE a b -> OrE (tinferE m t a) (tinferE m t b)
    XorE a b -> XorE (tinferE m t a) (tinferE m t b)
    ShlE a b -> ShlE (tinferE m t a) (tinferE m IntT b)
    ShrE a b -> ShrE (tinferE m t a) (tinferE m IntT b)
    -- TODO: type of MulE should be int
    MulE a b -> MulE (tinferE m t a) (tinferE m t b)
    NotE a -> NotE (tinferE m t a)
    HoleE _ -> HoleE t
    BitE {} -> x -- TODO: verify x has type t
    BitsE {} -> x -- TODO: verify x has type t
    IntE v ->
      -- The front end uses IntE for integer literals, but they may not have
      -- type int. Change to the appropriate expression if a different type is
      -- expected here.
      -- TODO: this is a bit hackish. Can we clean it up please?
      case t of
        IntT -> x
        BitT -> case v of
                  0 -> BitE False
                  1 -> BitE True
                  _ -> error $ "literal " ++ show v ++ " is too big for bit type"
        BitsT w -> BitsE (intB w v)
        _ -> error $ "integer literal used where non-int type expected"
                
    VarE {} -> x -- TODO: verify the variable has type t
                    -- TODO: don't use UnknownT here
    AccessE a b -> AccessE (tinferE m UnknownT a) (tinferE m IntT b)

