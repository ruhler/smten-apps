
module Program (
    FunctionKind(..), Decl(..), Program,
    program, decls,
    declT, declE,
    )
  where

import Smten.Prelude
import qualified Smten.Data.Map as Map

import Syntax

data FunctionKind =
    NormalF          -- ^ a normal function
  | WithSpecF Name   -- ^ the function has a spec
  | HarnessF         -- ^ the function is a harness
  | GeneratorF       -- ^ the function is a generator
 deriving (Show)

data Decl =
   FunD {
      d_name :: Name,
      fd_val :: Function,
      fd_kind :: FunctionKind }
 | VarD {
      vd_ty :: Type,
      d_name :: Name,
      vd_val :: Expr
   }
 deriving (Show)

type Program = Map.Map String Decl

-- Return the type of a declaration
declT :: Decl -> Type
declT d@(FunD {}) = f_type $ fd_val d
declT d@(VarD {}) = vd_ty d

-- Return the expression value of a declaration.
declE :: Decl -> Expr
declE x =
    case x of
        VarD {} -> vd_val x
        FunD {} -> ValE (FunV (fd_val x))

program :: [Decl] -> Program
program ds = Map.fromList [(d_name d, d) | d <- ds]

decls :: Program -> [Decl]
decls = Map.elems

