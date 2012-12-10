
import Grammar

import Control.Monad.State

main :: IO ()
main = do
    input <- getContents
    let r = runStateT hampi input
    putStrLn $ show r
        

