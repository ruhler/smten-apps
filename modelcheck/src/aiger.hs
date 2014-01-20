
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
  putStrLn $ show aig
  results <- runSMT yices2 $ mapM (aigercheck aig) (elems $ aig_badstates aig)
  putStrLn $ show results

