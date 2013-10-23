
module Input (
    ProgramInput, mkFreeProgramInput, mkFreeArg,
    ) where

import Smten.Prelude
import Smten.Control.Monad
import qualified Smten.Data.Map as Map
import Smten.Data.Functor
import Smten.Data.Maybe
import Smten.Symbolic

import Bits
import Eval
import Sketch

-- Construct a sample input for the given program.
mkFreeProgramInput :: ProgEnv -> Symbolic ProgramInput
mkFreeProgramInput env = do
  let p = declsof env
      mkDeclInput :: Decl -> Symbolic (Maybe (String, FunctionInput))
      mkDeclInput d@(FunD {}) =
        case fd_kind d of
          NormalF -> return Nothing
          GeneratorF -> return Nothing
          WithSpecF _ -> do
            i <- mkFreeArgs env (map fst (f_args . fd_val $ d))
            return $ Just (d_name d, i)
      mkDeclInput _ = return Nothing
  inputs <- mapM mkDeclInput p
  return $ Map.fromList (catMaybes inputs)

-- Given a list of types, return a list of free inputs corresponding to those
-- types.
mkFreeArgs :: ProgEnv -> [Type] -> Symbolic FunctionInput
mkFreeArgs env = mapM (mkFreeArg env)

-- Given a type, construct a free expression of that type.
mkFreeArg :: ProgEnv -> Type -> Symbolic Expr
mkFreeArg env t =
  case evalT env t of
    BitT -> BitE <$> free
    BitsT (IntE w) -> BitsE <$> freeBits w
    IntT -> IntE <$> freeInt

-- TODO: Cover more of the space of integers!
--       As determinied by appropriate input flags.
freeInt :: Symbolic Int
freeInt = msum (map return [0..10])

