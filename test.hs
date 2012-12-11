
import Data.Char(ord)

import RegEx

instance FromChar Int where
    fromChar = ord

abstar :: (FromChar c, Eq c) => RegEx c
abstar = starR (stringR "ab")

main :: IO ()
main = do
    putStrLn $ show (abstar :: RegEx Char)
    putStrLn . show $ match abstar "abab"

    putStrLn $ show (abstar :: RegEx Int)
    putStrLn . show $ match abstar (map ord "abab")

