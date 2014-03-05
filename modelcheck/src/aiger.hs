
import Smten.Prelude
import Smten.Data.Array
import Smten.Symbolic.SMT
import Smten.Symbolic.Solver.Yices2
import Aiger
import AigerPCheck

main :: IO ()
main = do
  txt <- getContents
  let aig = readAsciiAiger txt

  -- Note: we only look for the first output.
  -- Presumably we need to check for them all?
  result <- runSMT yices2 $ aigercheck aig (aig_outputs aig ! 1)
  case result of
    Nothing -> putStrLn "0\n."
    Just v -> do
       putStrLn "1"
       putStrLn "b0"
       putStrLn $ showVector (aig_reset aig)
       putStr . unlines $ map showVector v
       putStrLn "."

