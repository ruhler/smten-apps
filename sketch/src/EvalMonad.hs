
-- A monad used in evaluation.
module EvalMonad (
    EvalM, runEvalM, scope, assert, efail,
    lookupDecl, lookupVar, insertVar,
    getOutput, setOutput,
 ) where

import Smten.Prelude
import qualified Smten.Data.Map as Map
import Smten.Data.Functor
import Sketch

type Env = Map.Map Name Decl
type Vars = Map.Map Name Value

data SS = SS {
    -- | Local variables.
    ss_vars :: Map.Map Name Value,

    -- | The output of the statement if any.
    ss_out :: Value
}

-- The evaluation monad.
--  It has access to a read-only global environment,
--  it has access to a read/write local environment,
--  it has access to a read/write output value,
--  and it can fail.
newtype EvalM a = EvalM {
   runEvalM_ :: Env -> SS -> Maybe (a, SS)
}

instance Functor EvalM where
  fmap f x = x >>= (return . f)

instance Monad EvalM where
  return x = EvalM $ \_ s -> return (x, s)
  (>>=) x f = EvalM $ \e s -> do
      (v, s') <- runEvalM_ x e s
      runEvalM_ (f v) e s'

runEvalM :: ProgEnv -> EvalM a -> Maybe a
runEvalM env q =
  let s0 = SS Map.empty (error "evalM.ss_out._|_")
  in fst <$> runEvalM_ q env s0

-- Evaluate the monad in the given scope.
-- Outer scopes are not visible.
scope :: Map.Map Name Value -> EvalM a -> EvalM a
scope vars x = EvalM $ \e s -> do
  r <- runEvalM_ x e (SS vars (error "EvalM: no output value"))
  return (fst r, s)

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
lookupVar nm = EvalM $ \_ s -> return (Map.lookup nm (ss_vars s), s)

insertVar :: Name -> Value -> EvalM ()
insertVar nm val = EvalM $ \_ s ->
    return ((), s { ss_vars = Map.insert nm val (ss_vars s)})

getOutput :: EvalM Value
getOutput = EvalM $ \_ s -> return (ss_out s, s)

setOutput :: Value -> EvalM ()
setOutput v = EvalM $ \_ s -> return ((), s { ss_out = v })

