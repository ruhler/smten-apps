
import Smten.Prelude
import Smten.Control.Monad
import Smten.Symbolic
import Smten.Symbolic.Solver.Yices2
import RegEx

abstar :: RegEx Char 
abstar = starR (concatR (atomR 'a') (atomR 'b'))

chars :: [Char]
chars = ['a','b','c','d','e','f']

mkRegEx :: Int -> Symbolic (RegEx Char)
mkRegEx 0 = return emptyR
mkRegEx n = do
    x1 <- msum (map return chars)
    x2 <- msum (map return chars)
    a <- mkRegEx (n-1)
    b <- mkRegEx (n-1)
    msum (map return [epsilonR, rangeR x1 x2, concatR a b, orR a b, starR a])

-- Synthesize a regular expression which matches the given strings,
-- but not the other given strings.
resynN :: Int -> [String] -> [String] -> IO (Maybe (RegEx Char))
resynN n good bad =
   run_symbolic yices2 $ do
      re <- mkRegEx n
      guard $ all (match re) good
      guard $ all (not . match re) bad
      return re

resyn :: [String] -> [String] -> IO (RegEx Char)
resyn good bad =
  let f n = do
        r <- resynN n good bad
        case r of
            Just v -> return v
            Nothing -> f (n+1)
  in f 0

main :: IO ()
main = do
    txt <- getContents
    let f ('-':w) = ([], [w])
        f ('+':w) = ([w], [])
        f _ = ([], [])
    
        words = map f (lines txt)
        (goods, bads) = unzip words
    re <- resyn (concat goods) (concat bads)
    putStrLn (prettyc re)
