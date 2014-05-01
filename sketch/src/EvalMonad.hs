
-- A monad used in evaluation.
module EvalMonad (
    EvalM, runEvalM, scoped, assert, efail,
    lookupDecl, declVar, lookupVar, insertVar,
    newStruct, lookupStruct, updateStruct, lookupStructType,
 ) where

import Smten.Prelude
import Smten.Control.Monad
import qualified Smten.Data.Map as Map
import Smten.Data.Functor
import Smten.Data.Maybe

import Program
import Syntax

type Vars = Map.Map Name Value
type Addr = Int
type StructVal = Map.Map Name Value

-- The heap maps pointer locations into struct values.
type Heap = Map.Map Addr StructVal

data State = State {
  s_globals :: Vars,
  s_locals :: Vars,
  s_heap :: Heap
}

-- The evaluation monad.
--  It has access to a read-only  program environment,
--  It has access to a read/write global environment,
--  it has access to a read/write local environment,
--  it has access to a read/write heap,
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
runEvalM env q = fst <$> runEvalM_ q env (State Map.empty Map.empty Map.empty)

-- Evaluate the computation in a fresh local variable scope.
scoped :: EvalM a -> EvalM a
scoped x = EvalM $ \e s -> do
  (r, s') <- runEvalM_ x e (s { s_locals = Map.empty })
  return (r, s' { s_locals = s_locals s })

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
    return ((), s { s_locals = Map.insert nm val (s_locals s)})

-- Look for the given variable in the local or global scope
lookupVar :: Name -> EvalM (Maybe Value)
lookupVar nm = EvalM $ \_ s ->
   let mv = mplus (Map.lookup nm (s_locals s)) (Map.lookup nm (s_globals s))
   in return (mv, s)

-- Update the value of a variable.
-- If the variable is local, it updates the local scope.
-- If the variable is global, it updates the global scope.
--
-- If declVar has not been used to declare this variable, the variable is
-- assumed to be a global variable.
insertVar :: Name -> Value -> EvalM ()
insertVar nm val = EvalM $ \_ s ->
   if Map.member nm (s_locals s)
     then return ((), s { s_locals = Map.insert nm val (s_locals s)})
     else return ((), s { s_globals = Map.insert nm val (s_globals s)})

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

