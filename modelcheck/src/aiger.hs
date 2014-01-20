
import Smten.Prelude
import Smten.Data.Array
import Smten.Symbolic.SMT
import Smten.Symbolic.Solver.Yices2
import Aiger
import PCheck
import AigerPCheck

main :: IO ()
main = do
  txt <- getContents
  let aig = readAsciiAiger txt

  -- Note: we only look for the first bad state property.
  -- Presumably we need to check for them all?
  result <- runSMT yices2 $ aigercheck aig (aig_badstates aig ! 1)
  case result of
    Nothing -> putStrLn "0\n.\n"
    Just v -> do
       putStrLn "1"
       putStrLn "b0"
       putStrLn $ showVector (aig_reset aig)
       putStrLn . unlines $ map showVector v  ++ ["."]

