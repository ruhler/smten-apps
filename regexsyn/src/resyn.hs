
import Smten.Prelude
import Smten.Control.Monad
import Smten.Control.Monad.State
import Smten.Data.List
import Smten.Symbolic
import Smten.Symbolic.Solver.Yices2

import Grammar
import Ppr
import RegEx

abstar :: RegEx
abstar = starR (concatR (atomR 'a') (atomR 'b'))

mkRegEx :: [Char] -> Int -> Symbolic RegEx
mkRegEx _ 0 = return emptyR
mkRegEx alphabet n = do
    x1 <- msum (map return alphabet)
    x2 <- msum (map return alphabet)
    a <- mkRegEx alphabet (n-1)
    b <- mkRegEx alphabet (n-1)
    msum (map return [epsilonR, rangeR x1 x2, concatR a b, orR a b, starR a])

-- Synthesize a regular expression which matches the given strings,
-- but not the other given strings.
resynN :: [Char] -> Int -> [String] -> [String] -> IO (Maybe RegEx)
resynN alphabet n good bad =
   run_symbolic yices2 $ do
      re <- mkRegEx alphabet n
      guard $ all (match re) good
      guard $ all (not . match re) bad
      return re

resyn :: [Char] -> [String] -> [String] -> IO RegEx
resyn alphabet good bad =
  let f n = do
        r <- resynN alphabet n good bad
        case r of
            Just v -> return v
            Nothing -> f (n+1)
  in f 0

testparse :: String -> IO ()
testparse s = case evalStateT parseRegEx s of
                    Left msg -> fail msg
                    Right x -> putStrLn (pretty x)

main :: IO ()
main = do
    testparse "a"
    testparse "abc"
    testparse "a | b"
    testparse "(a | b)c"
    testparse "a | b | cd*e* | f"


    txt <- getContents
    let f ('-':w) = ([], [w])
        f ('+':w) = ([w], [])
        f _ = ([], [])
    
        words = map f (lines txt)
        (goods, bads) = unzip words
        goodwords = concat goods
        badwords = concat bads
        alphabet = nub (concat (goodwords ++ badwords))
    re <- resyn alphabet goodwords badwords
    putStrLn (pretty re)
