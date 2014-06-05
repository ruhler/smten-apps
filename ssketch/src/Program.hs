
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
      declN :: Name,
      fd_val :: Function,
      fd_kind :: FunctionKind }
 | VarD {
      vd_ty :: Type,
      declN :: Name,
      vd_val :: Expr
   }
 | StructD {
      declN :: Name,
      fields :: [(Name, Type)]
   }
 deriving (Show)

type Program = Map.Map String Decl

-- Return the type of a declaration
declT :: Decl -> Type
declT d@(FunD {}) = functionT $ fd_val d
declT d@(VarD {}) = vd_ty d
declT d@(StructD nm _) = StructT nm

-- Return the expression value of a declaration.
declE :: Decl -> Expr
declE x =
    case x of
        VarD {} -> vd_val x
        FunD {} -> ValE (FunV (fd_val x))
        StructD {} -> error "declE for StructD"

program :: [Decl] -> Program
program ds = Map.fromList [(declN d, d) | d <- ds]

decls :: Program -> [Decl]
decls = Map.elems

