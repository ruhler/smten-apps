
-- A monad used in evaluation.
module EvalMonad (
    EvalM, runEvalM, scope, assert,
    lookupDecl, lookupVar, insertVar,
    getOutput, setOutput,
 ) where

import Smten.Prelude
import Smten.Control.Monad.State
import qualified Smten.Data.Map as Map
import Sketch

data SS = SS {
    -- | The Global Environment
    ss_env :: Map.Map Name Decl,

    -- | Local variables.
    ss_vars :: Map.Map Name Value,

    -- | The output of the statement if any.
    ss_out :: Value,

    -- | Predicate which says if the result is valid.
    ss_valid :: Bool
}

-- The evaluation monad.
--  It has access to a read-only global environment,
--  it has access to a read/write local environment,
--  and it can fail.
type EvalM = State SS

runEvalM :: ProgEnv -> EvalM a -> Maybe a
runEvalM env q = 
  case runState q (SS env Map.empty (error "evalM.ss_out._|_") True) of
    (v, s) -> if (ss_valid s) then Just v else Nothing

-- Evaluate the monad in the given scope.
-- Outer scopes are not visible.
scope :: Map.Map Name Value -> EvalM a -> EvalM a
scope vars x = do
  olds <- get
  put (SS (ss_env olds) vars (error "scope.ss_out._|_") (ss_valid olds))
  r <- x
  modify $ \s -> s { ss_out = ss_out olds, ss_vars = ss_vars olds }
  return r

assert :: Bool -> EvalM ()
assert p = modify $ \s -> s { ss_valid = ss_valid s && p }

-- Look for the given declaration in the program environment.
lookupDecl :: Name -> EvalM (Maybe Decl)
lookupDecl nm = gets (Map.lookup nm . ss_env)

-- Look for the given variable in the local scope
lookupVar :: Name -> EvalM (Maybe Value)
lookupVar nm = gets (Map.lookup nm . ss_vars)

insertVar :: Name -> Value -> EvalM ()
insertVar nm val = modify $ \s -> s { ss_vars = Map.insert nm val (ss_vars s) }

getOutput :: EvalM Value
getOutput = gets ss_out

setOutput :: Value -> EvalM ()
setOutput v = modify $ \s -> s { ss_out = v }

