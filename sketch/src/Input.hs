
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
import Sketch

-- Construct a sample input for the given program.
mkFreeProgramInput :: Prog -> Symbolic ProgramInput
mkFreeProgramInput p = do
  let mkDeclInput :: Decl -> Symbolic (Maybe (String, FunctionInput))
      mkDeclInput d@(FunD {}) =
        case fd_spec d of
          Nothing -> return Nothing  -- No input needed for non-harness
          Just _ -> do
            i <- mkFreeArgs (map fst (fd_args d))
            return $ Just (d_name d, i)
      mkDeclInput _ = return Nothing
  inputs <- mapM mkDeclInput p
  return $ Map.fromList (catMaybes inputs)

-- Given a list of types, return a list of free inputs corresponding to those
-- types.
mkFreeArgs :: [Type] -> Symbolic FunctionInput
mkFreeArgs = mapM mkFreeArg

-- Given a type, construct a free expression of that type.
mkFreeArg :: Type -> Symbolic Expr
mkFreeArg BitT = BitE <$> free
mkFreeArg (BitsT (IntE w)) = BitsE <$> freeBits w
mkFreeArg IntT = IntE <$> freeInt

-- TODO: Cover more of the space of integers!
--       As determinied by appropriate input flags.
freeInt :: Symbolic Int
freeInt = msum (map return [0..10])

