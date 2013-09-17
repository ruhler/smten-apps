
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}

import Smten.Prelude
import Smten.Control.Monad.State
import Grammar

main :: IO ()
main = do
  input <- getContents
  sk <- case evalStateT parseSketch input of
            Left msg -> fail msg
            Right x -> return x
  putStrLn (show sk)

