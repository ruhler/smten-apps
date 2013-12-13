
import Smten.Prelude
import Block

test :: String -> Bool -> IO ()
test msg True = return ()
test msg False = error msg

main :: IO ()
main = do
  let block = fromRows ["abcd",
                        "efgh",
                        "ijkl"]

  test "rows" (["abcd", "efgh", "ijkl"] == rows block)
  test "cols" (["aei", "bfj", "cgk", "dhl"] == cols block)
  test "pdiags" (["a", "be", "cfi", "dgj", "hk", "l"] == pdiags block)
  test "ndiags" (["d", "ch", "bgl", "afk", "ej", "i"] == ndiags block)
  
