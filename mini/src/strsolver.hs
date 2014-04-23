
-- Example of a string constraint solver in Smten.

import Smten.Prelude
import Smten.Search
import Smten.Symbolic.Solver.Debug
import Smten.Symbolic.Solver.Yices2

data RegEx =
   Empty
 | Epsilon
 | Atom Char
 | Star RegEx
 | Concat RegEx RegEx
 | Or RegEx RegEx

match :: RegEx -> String -> Bool
match Empty _ = False
match Epsilon s = null s
match (Atom x) s = s == [x]
match r@(Star x) [] = True
match r@(Star x) s = any (match2 x r) (splits [1..length s] s)
match (Concat a b) s = any (match2 a b) (splits [0..length s] s)
match (Or a b) s = match a s || match b s

match2 :: RegEx -> RegEx -> (String, String) -> Bool
match2 a b (sa, sb) = match a sa && match b sb

splits :: [Int] -> [a] -> [([a], [a])]
splits ns x = map (\n -> splitAt n x) ns

data Template = Str String | Cat Template Template | Free Int

choose :: [a] -> Space a
choose [] = empty
choose [x] = single x
choose (x:xs) = union (single x) (choose xs)

mkString :: Template -> Space String
mkString (Str s) = single s
mkString (Cat ta tb) = do
  a <- mkString ta
  b <- mkString tb
  single (a ++ b)
mkString (Free 0) = single ""
mkString (Free w) = do
  hd <- choose ['a'..'z']
  tl <- mkString (Free (w-1))
  single (hd : tl)

mkSolution :: Template -> RegEx -> Space String
mkSolution t r = do
  s <- mkString t
  if (match r s)
    then single s
    else empty

solve :: Template -> RegEx -> IO ()
solve t r = do
  slv <- debug "build/strsolver.dbg" yices2
  result <- search slv (mkSolution t r)
  case result of
    Nothing -> putStrLn "No Solution"
    Just x -> putStrLn ("Solution: " ++ show x)

main :: IO ()
main = do
  let t = Cat (Str "ab") (Cat (Free 2) (Str "cd"))
      r = Concat (Star (Concat (Atom 'a') (Atom 'b')))
                 (Star (Concat (Atom 'c') (Atom 'd')))
  solve t r

