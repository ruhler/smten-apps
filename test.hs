
import RegEx

abstar :: RegEx
abstar = starR (stringR "ab")

main :: IO ()
main = do
    putStrLn $ show abstar
    putStrLn . show $ match abstar "abab"

