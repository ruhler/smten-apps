
module Cegis (cegis) where

import Smten.Prelude
import Smten.Control.Monad.Trans
import Smten.Searches

import Input
import Ppr
import Program

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
cegis :: Bool -> Space ProgramInput -> Space Program -> [ProgramInput] -> (Program -> ProgramInput -> Bool) -> Searches (Maybe Program)
cegis verbose freeX freeC xs p = do
  let info msg = if verbose then liftIO (putStrLn msg) else return ()
  -- Find some concrete 'c' which satisfies the predicate for all existing
  -- examples.
  info $ "searching for candidate satisfying: " ++ show xs
  rc <- search $ do
      c <- freeC
      guard (all (p c) xs)
      return c
  case rc of
    Nothing -> return Nothing
    Just cv -> do
      info $ "candidate program: " ++ pretty cv
      -- Search for a counter example
      rx <- search $ do
          x <- freeX
          guard (not (p cv x))
          return x
      case rx of
        Nothing -> return (Just cv)
        Just xv -> do
            info $ "found counter example: " ++ show xv
            cegis verbose freeX freeC (xv:xs) p
  
