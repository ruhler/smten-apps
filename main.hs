
import Grammar

import Control.Monad.State

main :: IO ()
main = do
    input <- getContents
    let r = runStateT hampi input
    case r of
        Left msg -> fail msg
        Right h -> putStrLn $ show h
        

