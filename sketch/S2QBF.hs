
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}
module S2QBF (s2qbf) where

import Smten.Prelude
import Smten.Symbolic
import Smten.Symbolic.SMT

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
s2qbf :: (Free a, Free b) => [a] -> (b -> a -> Bool) -> SMT (Maybe b)
s2qbf xs p = do
  -- Find some concrete 'c' which satisfies the predicate for all existing
  -- examples.
  rc <- query $ do
      c <- free
      assert (all (p c) xs)
      return c
  case rc of
    Nothing -> return Nothing
    Just cv -> do
      -- Search for a counter example
      rx <- query $ do
          x <- free
          assert (not (p cv x))
          return x
      case rx of
        Nothing -> return (Just cv)
        Just xv -> s2qbf (xv:xs) p
  
