
module Input (
    ProgramInput, mkFreeProgramInput, mkFreeArg,
    ) where

import Smten.Prelude
import qualified Smten.Data.Map as Map
import Smten.Data.Functor
import Smten.Data.Maybe
import Smten.Symbolic

import IntS
import Options
import Program
import Syntax

-- The input to a function is the list of its arguments.
type FunctionInput = [Value]

-- The input to a program is a sample function input for each of its top level
-- harnesses.
type ProgramInput = Map.Map String FunctionInput

-- Construct a sample input for the given program.
mkFreeProgramInput :: Options -> Program -> Symbolic ProgramInput
mkFreeProgramInput opts p = do
  let mkDeclInput :: Decl -> Symbolic (Maybe (String, FunctionInput))
      mkDeclInput d@(FunD {}) = do
        let needsinputs = 
             case fd_kind d of
               NormalF -> False
               GeneratorF -> False
               HarnessF -> True
               WithSpecF _ -> True
        if needsinputs 
            then case (functionT . fd_val $ d) of
                    FunT _ ts -> do
                      i <- mkFreeArgs opts ts
                      return $ Just (declN d, i)
            else return Nothing
      mkDeclInput _ = return Nothing
  inputs <- mapM mkDeclInput (decls p)
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
       arrayV <$> sequence (replicateS w (mkFreeArg bnd t))
    IntT -> IntV <$> freeInt bnd

