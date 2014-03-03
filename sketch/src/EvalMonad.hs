
-- A monad used in evaluation.
module EvalMonad (
    EvalM, runEvalM, scope, assert, efail,
    lookupDecl, lookupVar, insertVar,
 ) where

import Smten.Prelude
import qualified Smten.Data.Map as Map
import Smten.Data.Functor
import Sketch

type Env = Map.Map Name Decl
type Vars = Map.Map Name Value

type LocalVars = Map.Map Name Value

-- The evaluation monad.
--  It has access to a read-only global environment,
--  it has access to a read/write local environment,
--  it has access to a read/write output value,
--  and it can fail.
newtype EvalM a = EvalM {
   runEvalM_ :: Env -> LocalVars -> Maybe (a, LocalVars)
}

instance Functor EvalM where
  fmap f x = x >>= (return . f)

instance Monad EvalM where
  return x = EvalM $ \_ s -> return (x, s)
  (>>=) x f = EvalM $ \e s -> do
      (v, s') <- runEvalM_ x e s
      runEvalM_ (f v) e s'

runEvalM :: ProgEnv -> EvalM a -> Maybe a
runEvalM env q = fst <$> runEvalM_ q env Map.empty

-- Evaluate the monad in the given scope.
-- Outer scopes are not visible.
-- Returns the result of computation, and the local scope after computation
-- completes
scope :: LocalVars -> EvalM a -> EvalM (a, LocalVars)
scope vars x = EvalM $ \e s -> do
  r <- runEvalM_ x e vars
  return (r, s)

-- Evaluation which fails.
efail :: EvalM a
efail = EvalM $ \_ _ -> Nothing

assert :: Bool -> EvalM ()
assert True = return ()
assert False = efail

-- Look for the given declaration in the program environment.
lookupDecl :: Name -> EvalM (Maybe Decl)
lookupDecl nm = EvalM $ \e s -> return (Map.lookup nm e, s)

-- Look for the given variable in the local scope
lookupVar :: Name -> EvalM (Maybe Value)
lookupVar nm = EvalM $ \_ s -> return (Map.lookup nm s, s)

insertVar :: Name -> Value -> EvalM ()
insertVar nm val = EvalM $ \_ s ->
    return ((), Map.insert nm val s)

