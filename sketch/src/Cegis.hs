
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}
module Cegis (cegis) where

import Smten.Prelude
import Smten.Control.Monad.Trans
import Smten.Symbolic
import Smten.Symbolic.SMT

import Input
import Ppr
import Syntax

-- Answer the question:
--   Does there exist a value 'c' such that for all 'x', p(x, c) is satisfied?
--
--   Based on algorithm in section 5.4 of
--   "Combinatorial Sketching for Finite Programs", in ASPLOS06
--
--  You provide sample concrete values for 'x', it returns a value for 'c' if
--  it can. Presumably, the more sample values you provide the better, but you
--  needn't provide any if you don't want to.
--
--  x has type a
--  c has type b
cegis :: Symbolic ProgramInput -> Symbolic Prog -> [ProgramInput] -> (Prog -> ProgramInput -> Bool) -> SMT (Maybe Prog)
cegis freeX freeC xs p = do
  -- Find some concrete 'c' which satisfies the predicate for all existing
  -- examples.
  rc <- query $ do
      c <- freeC
      assert (all (p c) xs)
      return c
  case rc of
    Nothing -> return Nothing
    Just cv -> do
      liftIO $ putStrLn ("candidate program: " ++ pretty cv)
      -- Search for a counter example
      rx <- query $ do
          x <- freeX
          assert (not (p cv x))
          return x
      case rx of
        Nothing -> return (Just cv)
        Just xv -> do
            liftIO $ putStrLn ("found counter example: " ++ show xv)
            cegis freeX freeC (xv:xs) p
  
