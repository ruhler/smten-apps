
-- A monad used in evaluation.
module EvalMonad (
    EvalM, runEvalM, scoped, assert, efail,
    lookupDecl, lookupVar, insertVar,
    newStruct, lookupStruct, updateStruct, lookupStructType,
 ) where

import Smten.Prelude
import qualified Smten.Data.Map as Map
import Smten.Data.Functor
import Smten.Data.Maybe

import Program
import Syntax

type Vars = Map.Map Name Value

type LocalVars = Map.Map Name Value

-- The heap maps pointer locations into struct values.
type Heap = Map.Map Int (Map.Map Name Value)

data State = State {
  s_vars :: LocalVars,
  s_heap :: Heap
}

-- The evaluation monad.
--  It has access to a read-only global environment,
--  it has access to a read/write local environment,
--  it has access to a read/write heap,
--  it has access to a read/write output value,
--  and it can fail.
newtype EvalM a = EvalM {
   runEvalM_ :: Program -> State -> Maybe (a, State)
}

instance Functor EvalM where
  fmap f x = x >>= (return . f)

instance Monad EvalM where
  return x = EvalM $ \_ s -> return (x, s)
  (>>=) x f = EvalM $ \e s -> do
      (v, s') <- runEvalM_ x e s
      runEvalM_ (f v) e s'

runEvalM :: Program -> LocalVars -> EvalM a -> Maybe a
runEvalM env vars q = fst <$> runEvalM_ q env (State vars Map.empty)

-- Evaluate the monad in the given scope.
-- Outer scopes are not visible.
scoped :: LocalVars -> EvalM a -> EvalM a
scoped vars x = EvalM $ \e s -> do
  (r, State _ h') <- runEvalM_ x e (s { s_vars = vars })
  return (r, s { s_heap = h' })

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
lookupVar nm = EvalM $ \_ s -> return (Map.lookup nm (s_vars s), s)

insertVar :: Name -> Value -> EvalM ()
insertVar nm val = EvalM $ \_ s ->
    return ((), s { s_vars = Map.insert nm val (s_vars s)})

newStruct :: Map.Map Name Value -> EvalM Pointer
newStruct v = EvalM $ \_ s ->
   let ptr = 1 + Map.size (s_heap s)
       s' = s { s_heap = Map.insert ptr v (s_heap s) }
   in return (Pointer ptr, s')

lookupStruct :: Pointer -> EvalM (Map.Map Name Value)
lookupStruct Null = efail
lookupStruct (Pointer x) = EvalM $ \_ s ->
   return (fromMaybe (error "lookupStruct: invalid pointer") $ Map.lookup x (s_heap s), s)

updateStruct :: Pointer -> Map.Map Name Value -> EvalM ()
updateStruct Null _ = efail
updateStruct (Pointer ptr) v = EvalM $ \_ s ->
   return ((), s { s_heap = Map.insert ptr v (s_heap s) })
  
lookupStructType :: Name -> EvalM [(Name, Type)]
lookupStructType n = EvalM $ \e s ->
  case Map.lookup n e of
     Just (StructD _ fields) -> return (fields, s)

