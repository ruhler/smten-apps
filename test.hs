
import RegEx

abstar :: (FromChar c, Eq c) => RegEx c
abstar = starR (stringR "ab")

main :: IO ()
main = do
    putStrLn $ show (abstar :: RegEx Char)
    putStrLn . show $ match abstar "abab"

