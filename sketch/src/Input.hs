
module Input (
    ProgramInput, mkFreeProgramInput, mkFreeArg,
    ) where

import Smten.Prelude
import Smten.Control.Monad
import qualified Smten.Data.Map as Map
import Smten.Data.Functor
import Smten.Data.Maybe
import Smten.Symbolic

import Sketch

-- Construct a sample input for the given program.
mkFreeProgramInput :: Options -> Prog -> Symbolic ProgramInput
mkFreeProgramInput opts p = do
  let mkDeclInput :: Decl -> Symbolic (Maybe (String, FunctionInput))
      mkDeclInput d@(FunD {}) =
        case fd_kind d of
          NormalF -> return Nothing
          GeneratorF -> return Nothing
          WithSpecF _ -> do
            case (f_type . fd_val $ d) of
                FunT _ ts -> do
                  i <- mkFreeArgs opts ts
                  return $ Just (d_name d, i)
      mkDeclInput _ = return Nothing
  inputs <- mapM mkDeclInput p
  return $ Map.fromList (catMaybes inputs)

-- Given a list of types, return a list of free inputs corresponding to those
-- types.
mkFreeArgs :: Options -> [Type] -> Symbolic FunctionInput
mkFreeArgs opts = mapM (mkFreeArg (bnd_inbits opts))

-- Given a type, construct a free expression of that type.
-- Takes a bound on the number of bits used for the expression.
mkFreeArg :: Int -> Type -> Symbolic Value
mkFreeArg bnd t =
  case t of
    BitT -> BitV <$> free
    ArrT t (ValE (IntV w)) -> do
       arrayV <$> sequence (replicate w (mkFreeArg bnd t))
    IntT -> IntV <$> freeInt bnd

-- Compute 2^n
exp2 :: Int -> Int
exp2 0 = 1
exp2 n = 2 * exp2 (n-1)

freeInt :: Int -> Symbolic Int
freeInt n = msum (map return [0..(exp2 n - 1)])

