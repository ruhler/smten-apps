
import System.Environment
import System.Exit
import IntNQueensL

usage :: String
usage = "nqueensl [-e Int] <n>"

lookuparg :: String -> [String] -> Maybe String
lookuparg k m = 
  case dropWhile ((/=) k) m of
     (_:x:_) -> Just x
     _ -> Nothing

lookupn :: [String] -> Maybe Int
lookupn [] = Nothing
lookupn (('-':_):_:xs) = lookupn xs
lookupn (n:_) = Just (read n)

main :: IO ()
main = do
  args <- getArgs
  if "--help" `elem` args
     then putStrLn usage >> exitSuccess
     else return ()

  f <- case lookuparg "-e" args of
         Just "Int" -> return int_nqueensl
         Just x -> fail $ "Unknown elem type: " ++ x ++ ".\n" ++ usage
         Nothing -> return int_nqueensl

  n <- case lookupn args of
          Nothing -> fail $ "no board size input\n" ++ usage 
          Just v -> return v
  f n

