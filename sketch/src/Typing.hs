
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}
module Typing (tinferP) where

import Smten.Prelude
import qualified Smten.Data.Map as Map

import Sketch

type TypeEnv = Map.Map Name Type

-- Perform type inference and checking on the given program.
-- Returns the type inferred program, or goes BOOM if there's a type error.
-- TODO: We probably ought to handle errors in a nicer way.
-- TODO: take the specs map as input rather than duplicate the work to create
-- it here and in synthesis?
tinferP :: Prog -> Prog
tinferP p = 
  let specs = Map.fromList [(fd_name x, x) | x <- p]
  in map (tinferD specs) p

tinferD :: Map.Map Name Decl -> Decl -> Decl
tinferD specs d =
   let m = Map.fromList (map (\(a, b) -> (b, a)) $ fd_args d) 
   in d { fd_stmts = map (tinferS m (fd_outty d)) (fd_stmts d)}

tinferS :: TypeEnv -> Type -> Stmt -> Stmt
tinferS m t (ReturnS x) = ReturnS (tinferE m t x)

-- tinferE tyenv tytgt x
--   tyenv - the type environment
--   tytgt - the asserted type of this expression
--   x - the expression to run inference on
tinferE :: TypeEnv -> Type -> Expr -> Expr
tinferE m t x =
  case x of
    -- TODO: for AndE and OrE: verify the target type is bit or bit[n] or
    -- whatever else it can be.
    AndE a b -> AndE (tinferE m t a) (tinferE m t b)
    OrE a b -> OrE (tinferE m t a) (tinferE m t b)
    HoleE _ -> HoleE t
    BitE {} -> x -- TODO: verify x has type t
    BitsE {} -> x -- TODO: verify x has type t
    IntE {} -> x -- TODO: verify x has type t
    VarE {} -> x -- TODO: verify the variable has type t
                    -- TODO: don't use UnknownT here
    AccessE a b -> AccessE (tinferE m UnknownT a) (tinferE m IntT b)

