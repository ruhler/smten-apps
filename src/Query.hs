
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}
module Query (hquery) where

import Smten.Prelude
import Smten.Data.Array
import Smten.Data.List (isInfixOf)
import Smten.Data.Maybe (fromMaybe)
import qualified Smten.Data.Map as Map
import Smten.Symbolic

import SChar
import Hampi
import RegEx
import CFG
import Fix
import Match

-- Ignores value of the first argument. That's just to specify the type.
freevar :: (SChar c) => c -> Int -> Symbolic [c]
freevar _ = freeSCharString

-- inlinevals varid varval vals
--  Given the var id, it's symbolic value, and the rest of the values, return
--  a mapping from id to totally inlined values.
inlinevals :: (SChar c) => ID -> [c] -> [(ID, Val)] -> Map.Map ID [c]
inlinevals varid varval m =
  let -- lookupval :: Val -> Maybe [c]
      lookupval (ValID x)
        | x == varid = return varval
        | otherwise = lookup x m >>= lookupval
      lookupval (ValLit x) = return $ toSCharString x
      lookupval (ValCat a b) = do
            a' <- lookupval a
            b' <- lookupval b
            return $ a' ++ b'
      lookupval (ValSub src off len) = do
            src' <- lookupval (ValID src)
            return $ substring src' off len
      vals = [(id, fromMaybe (error $ show v ++ " not found") $ lookupval v) | (id, v) <- m]
  in Map.fromList $ (varid, varval) : vals

-- substring src offset length
substring :: [a] -> Int -> Int -> [a]
substring s o l = take l $ drop o s

contains :: (SChar c) => [c] -> String -> Bool
contains v s = isInfixOf (toSCharString s) v

xor :: Bool -> Bool -> Bool
xor b p = if b then p else not p

assertIn :: (SChar c) => FixResult -> [c] -> Bool
assertIn fr vstr =
 let regsbnd = fr_regbound fr
     regs = fr_regs fr
     reg = fr_top fr
 in match (array regsbnd regs) reg vstr

-- Make a hampi assertion.
hassert :: (SChar c) => Map.Map ID [c] -> Assertion -> Symbolic ()
hassert vals (AssertIn v b f) =
    let vstr = fromMaybe (error $ "val " ++ v ++ " not found") $ Map.lookup v vals
        fr = f (length vstr)
    in assert $ xor b (assertIn fr vstr)

hassert vals (AssertEquals v b x) =
    let vstr = fromMaybe (error $ "val " ++ v ++ " not found") $ Map.lookup v vals
        xstr = fromMaybe (error $ "val " ++ x ++ " not found") $ Map.lookup x vals
    in assert $ xor b (vstr == xstr)

hassert vals (AssertContains v b s) =
    let vstr = fromMaybe (error $ "val " ++ v ++ " not found") $ Map.lookup v vals
    in assert $ xor b (contains vstr s)

-- A hampi query.
-- Takes an argument of SChar type to specify which type to use for the
-- element. The value of that argument is ignored.
hquery :: (SChar c) => c -> Solver -> Hampi -> IO String
hquery _ _ (Hampi (Var vid wmin wmax) _ _) | wmax < wmin = return "UNSAT"
hquery e s (Hampi (Var vid wmin wmax) vals asserts) = do
    r <- run_symbolic s $ do
        svar <- freevar e wmin
        let svals = inlinevals vid svar vals
        mapM_ (hassert svals) asserts
        return svar
    case r of
        Just v ->
          let vstr = fromSCharString v
          in return $ "{VAR(" ++ vid ++ ")=" ++ vstr ++ "}"
        Nothing -> hquery e s (Hampi (Var vid (wmin + 1) wmax) vals asserts)
