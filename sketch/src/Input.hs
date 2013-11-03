
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
            case (f_type . fd_val $ d) of
                FunT _ ts -> do
                  i <- mkFreeArgs env ts
                  return $ Just (d_name d, i)
      mkDeclInput _ = return Nothing
  inputs <- mapM mkDeclInput p
  return $ Map.fromList (catMaybes inputs)

-- Given a list of types, return a list of free inputs corresponding to those
-- types.
mkFreeArgs :: ProgEnv -> [Type] -> Symbolic FunctionInput
mkFreeArgs env = mapM (mkFreeArg env bnd_ctrlbits)

-- Given a type, construct a free expression of that type.
-- Takes a bound on the number of bits used for the expression.
mkFreeArg :: ProgEnv -> Int -> Type -> Symbolic Value
mkFreeArg env bnd t =
  case evalT env t of
    BitT -> BitV <$> free
    ArrT BitT (ValE (IntV w)) -> BitsV <$> freeBits w bnd
    IntT -> IntV <$> freeInt bnd

-- Compute 2^n
exp2 :: Int -> Int
exp2 0 = 1
exp2 n = 2 * exp2 (n-1)

freeInt :: Int -> Symbolic Int
freeInt n = msum (map return [0..(exp2 n - 1)])

