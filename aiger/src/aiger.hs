
import Smten.Prelude
import Aiger

main :: IO ()
main = do
  txt <- getContents
  let aig = readAsciiAiger txt
  putStrLn $ show aig

