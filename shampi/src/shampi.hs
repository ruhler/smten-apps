
import Smten.Prelude
import Smten.Data.Maybe(fromMaybe)
import Smten.Data.Functor((<$>))
import Smten.Symbolic.Solver.Debug
import Smten.Symbolic.Solver.STP
import Smten.Symbolic.Solver.MiniSat
import Smten.Symbolic.Solver.Yices1
import Smten.Symbolic.Solver.Yices2
import Smten.Symbolic.Solver.Z3
import Smten.System.Environment
import Smten.System.Exit
import Smten.System.IO
import Smten.System.Timeout

import Lexer
import Grammar
import Query
import SChar

data SCharType = SChar_Integer | SChar_Bit | SChar_Char | SChar_Int
    deriving (Eq, Show)

lookuparg :: String -> [String] -> Maybe String
lookuparg k m = 
  case dropWhile ((/=) k) m of
     (_:x:_) -> Just x
     _ -> Nothing

getfiles :: [String] -> [String]
getfiles [] = []
getfiles (x:xs) 
  | head x == '-' = getfiles (drop 1 xs)
  | otherwise = x:xs

usage :: String
usage = "Usage: shampi [-t timeout(s)] [-d debug] [-s yices1 | yices2 | stp | z3 | minisat] [-e Integer | Bit] [FILE]"

main :: IO ()
main = do
    args <- getArgs

    if "--help" `elem` args
        then putStrLn usage >> exitSuccess
        else return ()

    let to = fromMaybe (negate 1) (read <$> lookuparg "-t" args)
    basesolver <- case lookuparg "-s" args of
                     Just "yices1" -> return yices1
                     Just "yices2" -> return yices2
                     Just "stp" -> return stp
                     Just "minisat" -> return minisat
                     Just "z3" -> return z3
                     Just x -> fail $ "Unknown solver: " ++ x ++ ".\n" ++ usage
                     Nothing -> return yices2

    solver <- case lookuparg "-d" args of
                     Just fn -> debug fn basesolver
                     Nothing -> return basesolver

    elemtype <- case lookuparg "-e" args of
                 Just "Integer" -> return SChar_Integer
                 Just "Bit" -> return SChar_Bit
                 Just "Char" -> return SChar_Char
                 Just "Int" -> return SChar_Int
                 Just x -> fail $ "Unknown elem type: " ++ x ++ ".\n" ++ usage
                 Nothing -> return SChar_Bit

    let fins = getfiles args
        runf fin = do
            putStr (fin ++ ": ")
            input <- readFile fin

            let h = {-# SCC "ParseHampi" #-} parseHampi (lexer input)
                hq = case elemtype of
                        SChar_Bit -> hquery bitSChar solver h
                        SChar_Integer -> hquery integerSChar solver h
                        SChar_Char -> hquery charSChar solver h
                        SChar_Int -> hquery intSChar solver h
            r <- timeout (1000000*to) hq
            putStrLn (fromMaybe "TIMEOUT" r)
    mapM_ runf fins

