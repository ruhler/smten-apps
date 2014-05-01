
-- A monad used in evaluation.
module EvalMonad (
    EvalM, runEvalM, scoped, assert, efail,
    lookupDecl, declVar, lookupVar, insertVar,
    newStruct, lookupStruct, updateStruct, lookupStructType,
 ) where

import Smten.Prelude
import qualified Smten.Data.Map as Map
import Smten.Data.Functor
import Smten.Data.Maybe

import Program
import Syntax

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

runEvalM :: Program -> EvalM a -> Maybe a
runEvalM env q = fst <$> runEvalM_ q env (State Map.empty Map.empty)

-- Evaluate the computation in a fresh local variable scope.
scoped :: EvalM a -> EvalM a
scoped x = EvalM $ \e s -> do
  (r, s') <- runEvalM_ x e (s { s_vars = Map.empty })
  return (r, s' { s_vars = s_vars s })

-- Evaluation which fails.
efail :: EvalM a
efail = EvalM $ \_ _ -> Nothing

assert :: Bool -> EvalM ()
assert True = return ()
assert False = efail

-- Look for the given declaration in the program environment.
lookupDecl :: Name -> EvalM (Maybe Decl)
lookupDecl nm = EvalM $ \e s -> return (Map.lookup nm e, s)

-- Declare a variable as a local variable with given initial value.
declVar :: Name -> Value -> EvalM ()
declVar nm val = EvalM $ \_ s ->
    return ((), s { s_vars = Map.insert nm val (s_vars s)})

-- Look for the given variable in the local scope
lookupVar :: Name -> EvalM (Maybe Value)
lookupVar nm = EvalM $ \_ s -> return (Map.lookup nm (s_vars s), s)

-- Update the value of a variable.
-- If the variable is local, it updates the local scope.
-- If the variable is global, it updates the global scope.
--
-- If declVar has not been used to declare this variable, the variable is
-- assumed to be a global variable.
insertVar :: Name -> Value -> EvalM ()
insertVar nm val = EvalM $ \_ s ->
    if Map.member nm (s_vars s)
      then return ((), s { s_vars = Map.insert nm val (s_vars s)})
      else error ("insertVar: TODO: support nolocal var insert: " ++ nm)

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

